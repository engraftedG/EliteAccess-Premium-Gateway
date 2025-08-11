;; =====================================================================================
;; EliteAccess Premium Gateway - Token-Gated Subscription Management Protocol
;; =====================================================================================
;;
;; Advanced subscription infrastructure combining recurring payment mechanisms
;; with sophisticated token-based access control for premium service delivery
;; Supports multi-tier access levels and automated subscription lifecycle management

;; =====================================================================================
;; SYSTEM CONFIGURATION & CONSTANTS
;; =====================================================================================

;; Platform administration authority
(define-constant gateway-controller tx-sender)

;; Subscription tier definitions with pricing structure
(define-constant tier-basic u1)
(define-constant tier-premium u2)
(define-constant tier-elite u3)

;; Temporal parameters for subscription cycles (in blocks)
(define-constant subscription-duration-monthly u4320)    ;; Approximately 30 days
(define-constant subscription-duration-quarterly u12960) ;; Approximately 90 days
(define-constant subscription-duration-annual u52560)    ;; Approximately 365 days

;; Grace period buffer for payment processing delays
(define-constant payment-grace-period u144) ;; Approximately 1 day

;; =====================================================================================
;; ERROR HANDLING FRAMEWORK
;; =====================================================================================

(define-constant error-unauthorized-administrator (err u400))
(define-constant error-subscription-nonexistent (err u401))
(define-constant error-insufficient-payment-amount (err u402))
(define-constant error-subscription-already-active (err u403))
(define-constant error-token-requirement-unmet (err u404))
(define-constant error-subscription-expired (err u405))
(define-constant error-invalid-tier-selection (err u406))
(define-constant error-payment-processing-failed (err u407))
(define-constant error-token-balance-insufficient (err u408))
(define-constant error-access-level-restricted (err u409))
(define-constant error-subscription-renewal-failed (err u410))

;; Additional error constants for input validation
(define-constant error-invalid-tier-name (err u420))
(define-constant error-invalid-monthly-price (err u421))
(define-constant error-invalid-token-balance (err u422))
(define-constant error-invalid-access-level (err u423))
(define-constant error-invalid-token-type (err u424))
(define-constant error-invalid-holding-period (err u425))
(define-constant error-invalid-tier-boost (err u426))
(define-constant error-invalid-duration (err u427))
(define-constant error-invalid-principal (err u428))

;; =====================================================================================
;; CORE DATA STRUCTURES
;; =====================================================================================

;; Comprehensive subscription management registry
(define-map premium-member-registry
  { subscriber-address: principal }
  {
    membership-tier: uint,
    activation-timestamp: uint,
    expiration-boundary: uint,
    payment-amount-locked: uint,
    renewal-preference: bool,
    access-token-required: principal,
    subscription-status: uint ;; 0=inactive, 1=active, 2=expired, 3=suspended
  }
)

;; Service tier configuration and pricing matrix
(define-map service-tier-configuration
  { tier-identifier: uint }
  {
    tier-designation: (string-ascii 32),
    monthly-cost-micro-stx: uint,
    required-token-contract: (optional principal),
    minimum-token-balance: uint,
    feature-access-level: uint,
    tier-availability: bool
  }
)

;; Token-based access verification registry
(define-map verified-access-tokens
  { token-contract-address: principal }
  {
    token-standard: (string-ascii 16), ;; "sip009" or "sip010"
    verification-active: bool,
    minimum-holding-period: uint,
    tier-eligibility-boost: uint
  }
)

;; Revenue collection and withdrawal tracking
(define-map platform-treasury-ledger
  { collection-period: uint }
  {
    total-revenue-collected: uint,
    active-subscriptions-count: uint,
    withdrawal-processed: bool,
    accounting-timestamp: uint
  }
)

;; =====================================================================================
;; ADMINISTRATIVE STATE VARIABLES
;; =====================================================================================

;; Sequential subscription identification system
(define-data-var subscription-sequence-counter uint u0)

;; Platform operational status controls
(define-data-var gateway-operational-status bool true)

;; Emergency pause mechanism for critical situations
(define-data-var emergency-pause-activated bool false)

;; Revenue accumulator for platform earnings
(define-data-var accumulated-platform-revenue uint u0)

;; =====================================================================================
;; APPROVED PRINCIPALS REGISTRY
;; =====================================================================================

;; Registry of approved token contracts (for validation)
(define-map approved-token-contracts
  { contract-address: principal }
  { approved: bool })

;; Function to approve token contracts (admin only)
(define-public (approve-token-contract (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender gateway-controller) error-unauthorized-administrator)
    (map-set approved-token-contracts { contract-address: token-contract } { approved: true })
    (ok true)))

;; Function to revoke token contract approval
(define-public (revoke-token-contract (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender gateway-controller) error-unauthorized-administrator)
    (map-delete approved-token-contracts { contract-address: token-contract })
    (ok true)))

;; =====================================================================================
;; INPUT VALIDATION FUNCTIONS
;; =====================================================================================

;; Validate tier name is not empty and within length limits
(define-private (validate-tier-name (name (string-ascii 32)))
  (and 
    (> (len name) u0)
    (<= (len name) u32)))

;; Validate monthly price is within reasonable bounds
(define-private (validate-monthly-price (price uint))
  (and 
    (> price u0)
    (<= price u1000000000000))) ;; 1,000,000 STX maximum (in micro-STX)

;; Validate access level is within defined range
(define-private (validate-access-level (level uint))
  (and 
    (>= level u1)
    (<= level u100))) ;; Access level range 1-100

;; Validate tier ID is within acceptable range
(define-private (validate-tier-id (tier uint))
  (and 
    (>= tier tier-basic)
    (<= tier tier-elite)))

;; Validate token balance requirements
(define-private (validate-token-balance (balance uint))
  (<= balance u1000000000000000000)) ;; Reasonable maximum token balance

;; Validate duration is within acceptable bounds
(define-private (validate-duration-blocks (duration uint))
  (and 
    (> duration u0)
    (<= duration u525600))) ;; Maximum ~10 years in blocks

;; Validate holding period is reasonable
(define-private (validate-holding-period (period uint))
  (<= period u52560)) ;; Maximum 1 year

;; Validate tier boost percentage
(define-private (validate-tier-boost (boost uint))
  (<= boost u1000)) ;; Maximum 1000% boost

;; Validate token type is supported standard
(define-private (validate-token-type (token-type (string-ascii 16)))
  (or 
    (is-eq token-type "sip009")
    (is-eq token-type "sip010")))

;; Validate renewal duration is reasonable
(define-private (validate-renewal-duration (duration uint))
  (and 
    (> duration u0)
    (<= duration u525600))) ;; Maximum ~10 years

;; NEW: Validate that a principal is an approved token contract
(define-private (validate-token-contract (token-contract principal))
  (default-to false 
    (get approved (map-get? approved-token-contracts { contract-address: token-contract }))))

;; =====================================================================================
;; SUBSCRIPTION TIER INITIALIZATION FUNCTIONS
;; =====================================================================================

;; Administrative function to configure service tier parameters
(define-public (configure-service-tier 
    (tier-id uint) 
    (tier-name (string-ascii 32))
    (monthly-price uint)
    (token-contract (optional principal))
    (min-token-balance uint)
    (access-level uint))
  (begin
    ;; Verify administrative privileges for tier configuration
    (asserts! (is-eq tx-sender gateway-controller) error-unauthorized-administrator)

    ;; Validate all input parameters
    (asserts! (validate-tier-id tier-id) error-invalid-tier-selection)
    (asserts! (validate-tier-name tier-name) error-invalid-tier-name)
    (asserts! (validate-monthly-price monthly-price) error-invalid-monthly-price)
    (asserts! (validate-token-balance min-token-balance) error-invalid-token-balance)
    (asserts! (validate-access-level access-level) error-invalid-access-level)

    ;; FIXED: Validate token contract if provided
    (asserts! 
      (match token-contract
        some-token (validate-token-contract some-token)
        true) ;; If none provided, validation passes
      error-invalid-principal)

    ;; Store comprehensive tier configuration
    (map-set service-tier-configuration
      { tier-identifier: tier-id }
      {
        tier-designation: tier-name,
        monthly-cost-micro-stx: monthly-price,
        required-token-contract: token-contract,
        minimum-token-balance: min-token-balance,
        feature-access-level: access-level,
        tier-availability: true
      })

    (ok tier-id)))

;; Register approved token contracts for access verification
(define-public (register-access-token 
    (token-address principal)
    (token-type (string-ascii 16))
    (holding-period uint)
    (tier-boost uint))
  (begin
    ;; Ensure only platform administrator can register tokens
    (asserts! (is-eq tx-sender gateway-controller) error-unauthorized-administrator)

    ;; Validate all input parameters
    (asserts! (validate-token-type token-type) error-invalid-token-type)
    (asserts! (validate-holding-period holding-period) error-invalid-holding-period)
    (asserts! (validate-tier-boost tier-boost) error-invalid-tier-boost)

    ;; FIXED: Validate token contract address
    (asserts! (validate-token-contract token-address) error-invalid-principal)

    ;; Store token verification parameters
    (map-set verified-access-tokens
      { token-contract-address: token-address }
      {
        token-standard: token-type,
        verification-active: true,
        minimum-holding-period: holding-period,
        tier-eligibility-boost: tier-boost
      })

    (ok token-address)))

;; =====================================================================================
;; SUBSCRIPTION LIFECYCLE MANAGEMENT
;; =====================================================================================

;; Primary subscription creation with token verification
(define-public (initiate-premium-subscription 
    (selected-tier uint)
    (duration-blocks uint)
    (token-verification-address (optional principal)))
  (begin
    ;; Validate input parameters early
    (asserts! (validate-tier-id selected-tier) error-invalid-tier-selection)
    (asserts! (validate-duration-blocks duration-blocks) error-invalid-duration)

    ;; FIXED: Validate token verification address if provided
    (asserts!
      (match token-verification-address
        some-token (validate-token-contract some-token)
        true) ;; If none provided, validation passes
      error-invalid-principal)

    (let
      ((current-timestamp block-height)
       (subscriber-principal tx-sender)
       (tier-configuration (unwrap! (map-get? service-tier-configuration { tier-identifier: selected-tier }) error-invalid-tier-selection))
       (subscription-cost (get monthly-cost-micro-stx tier-configuration))
       (calculated-duration (calculate-subscription-duration duration-blocks))
       (total-payment-required (calculate-total-payment subscription-cost calculated-duration))
       ;; FIXED: Use validated token address with safe default
       (validated-token-address (match token-verification-address
                                  some-token some-token
                                  tx-sender))) ;; Safe default

      ;; Verify subscription service availability
      (asserts! (var-get gateway-operational-status) error-access-level-restricted)
      (asserts! (not (var-get emergency-pause-activated)) error-access-level-restricted)

      ;; Validate subscription tier availability
      (asserts! (get tier-availability tier-configuration) error-invalid-tier-selection)

      ;; Check for existing active subscription
      (asserts! (is-none (get-active-subscription subscriber-principal)) error-subscription-already-active)

      ;; Process token verification if required
      (match token-verification-address
        token-contract
        (try! (verify-token-eligibility subscriber-principal token-contract selected-tier))
        true)

      ;; Execute payment processing
      (try! (stx-transfer? total-payment-required subscriber-principal (as-contract tx-sender)))

      ;; Create comprehensive subscription record
      (map-set premium-member-registry
        { subscriber-address: subscriber-principal }
        {
          membership-tier: selected-tier,
          activation-timestamp: current-timestamp,
          expiration-boundary: (+ current-timestamp calculated-duration),
          payment-amount-locked: total-payment-required,
          renewal-preference: false,
          access-token-required: validated-token-address,
          subscription-status: u1
        })

      ;; Update platform revenue tracking
      (var-set accumulated-platform-revenue 
        (+ (var-get accumulated-platform-revenue) total-payment-required))

      ;; Increment subscription counter for analytics
      (var-set subscription-sequence-counter 
        (+ (var-get subscription-sequence-counter) u1))

      (ok { 
        subscription-id: (var-get subscription-sequence-counter),
        tier: selected-tier,
        expires-at: (+ current-timestamp calculated-duration)
      }))))

;; Subscription renewal mechanism with automatic payment processing
(define-public (renew-subscription-membership (renewal-duration uint))
  (begin
    ;; Validate renewal duration input
    (asserts! (validate-renewal-duration renewal-duration) error-invalid-duration)

    (let
      ((subscriber-address tx-sender)
       (existing-subscription (unwrap! (map-get? premium-member-registry { subscriber-address: subscriber-address }) error-subscription-nonexistent))
       (current-tier (get membership-tier existing-subscription))
       (tier-config (unwrap! (map-get? service-tier-configuration { tier-identifier: current-tier }) error-invalid-tier-selection))
       (renewal-cost (get monthly-cost-micro-stx tier-config))
       (calculated-extension (calculate-subscription-duration renewal-duration))
       (total-renewal-payment (calculate-total-payment renewal-cost calculated-extension))
       (current-expiration (get expiration-boundary existing-subscription))
       (new-expiration (+ current-expiration calculated-extension)))

      ;; Verify subscription exists and is renewable
      (asserts! (> (get subscription-status existing-subscription) u0) error-subscription-nonexistent)

      ;; Process renewal payment
      (try! (stx-transfer? total-renewal-payment subscriber-address (as-contract tx-sender)))

      ;; Update subscription with extended duration
      (map-set premium-member-registry
        { subscriber-address: subscriber-address }
        (merge existing-subscription {
          expiration-boundary: new-expiration,
          payment-amount-locked: (+ (get payment-amount-locked existing-subscription) total-renewal-payment),
          subscription-status: u1
        }))

      ;; Update platform revenue
      (var-set accumulated-platform-revenue 
        (+ (var-get accumulated-platform-revenue) total-renewal-payment))

      (ok new-expiration))))

;; =====================================================================================
;; ACCESS VERIFICATION FUNCTIONS
;; =====================================================================================

;; Comprehensive access validation combining subscription and token requirements
(define-read-only (validate-premium-access (user-address principal) (required-access-level uint))
  (match (map-get? premium-member-registry { subscriber-address: user-address })
    subscription-record
    (let
      ((subscription-active (is-subscription-currently-active subscription-record))
       (tier-level (get membership-tier subscription-record))
       (tier-config (map-get? service-tier-configuration { tier-identifier: tier-level })))

      (and 
        subscription-active
        (match tier-config
          config (>= (get feature-access-level config) required-access-level)
          false)))
    false))

;; Token ownership verification for enhanced access control
(define-read-only (verify-token-ownership (holder-address principal) (token-contract principal))
  (match (map-get? verified-access-tokens { token-contract-address: token-contract })
    token-config
    (and 
      (get verification-active token-config)
      ;; Note: Actual token balance check would require integration with specific token contracts
      ;; This is a placeholder for token verification logic
      true)
    false))

;; =====================================================================================
;; UTILITY AND HELPER FUNCTIONS
;; =====================================================================================

;; Calculate subscription duration based on selected period
(define-read-only (calculate-subscription-duration (period-selection uint))
  (if (is-eq period-selection u1)
    subscription-duration-monthly
    (if (is-eq period-selection u3)
      subscription-duration-quarterly
      subscription-duration-annual)))

;; Calculate total payment based on cost and duration
(define-read-only (calculate-total-payment (base-cost uint) (duration uint))
  (* base-cost (/ duration subscription-duration-monthly)))

;; Check if subscription is currently active and not expired
(define-read-only (is-subscription-currently-active (subscription-record (tuple (membership-tier uint) (activation-timestamp uint) (expiration-boundary uint) (payment-amount-locked uint) (renewal-preference bool) (access-token-required principal) (subscription-status uint))))
  (and 
    (is-eq (get subscription-status subscription-record) u1)
    (> (get expiration-boundary subscription-record) block-height)))

;; Retrieve active subscription details for a user
(define-read-only (get-active-subscription (user-address principal))
  (map-get? premium-member-registry { subscriber-address: user-address }))

;; Token eligibility verification with tier requirements
(define-private (verify-token-eligibility (user-address principal) (token-contract principal) (target-tier uint))
  (begin
    ;; Validate inputs
    (asserts! (validate-tier-id target-tier) error-invalid-tier-selection)
    (asserts! (validate-token-contract token-contract) error-invalid-principal)

    (let
      ((token-config (unwrap! (map-get? verified-access-tokens { token-contract-address: token-contract }) error-token-requirement-unmet))
       (tier-config (unwrap! (map-get? service-tier-configuration { tier-identifier: target-tier }) error-invalid-tier-selection)))

      ;; Verify token is active and user meets requirements
      (asserts! (get verification-active token-config) error-token-requirement-unmet)

      ;; Additional token balance verification would be implemented here
      ;; depending on the specific token standard integration

      (ok true))))

;; =====================================================================================
;; ADMINISTRATIVE FUNCTIONS
;; =====================================================================================

;; Check if a token contract is approved
(define-read-only (is-token-contract-approved (token-contract principal))
  (default-to false 
    (get approved (map-get? approved-token-contracts { contract-address: token-contract }))))

;; Get list of subscription details (for admin purposes)
(define-read-only (get-subscription-details (user-address principal))
  (map-get? premium-member-registry { subscriber-address: user-address }))

;; Get tier configuration details
(define-read-only (get-tier-configuration (tier-id uint))
  (map-get? service-tier-configuration { tier-identifier: tier-id }))

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-subscriptions: (var-get subscription-sequence-counter),
    total-revenue: (var-get accumulated-platform-revenue),
    operational-status: (var-get gateway-operational-status),
    emergency-pause: (var-get emergency-pause-activated)
  })