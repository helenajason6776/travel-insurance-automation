
;; title: travel-insurance
;; version: 1.0.0
;; summary: Automated travel insurance with policy activation, flight delay compensation, and emergency assistance
;; description: Smart contract system for managing travel insurance policies with automatic activation, 
;;              flight delay compensation, and emergency assistance coordination based on travel data

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-policy (err u103))
(define-constant err-insufficient-coverage (err u104))
(define-constant err-claim-expired (err u105))
(define-constant err-claim-already-processed (err u106))

;; Policy status constants
(define-constant policy-inactive u0)
(define-constant policy-active u1)
(define-constant policy-expired u2)

;; Claim status constants
(define-constant claim-pending u0)
(define-constant claim-approved u1)
(define-constant claim-rejected u2)

;; data vars
(define-data-var policy-counter uint u0)
(define-data-var claim-counter uint u0)

;; data maps
(define-map policies
  { policy-id: uint }
  { 
    holder: principal,
    premium: uint,
    coverage-amount: uint,
    start-block: uint,
    end-block: uint,
    destination: (string-ascii 64),
    flight-number: (string-ascii 16),
    status: uint,
    emergency-contact: (string-ascii 128)
  }
)

(define-map claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    claim-type: (string-ascii 32),
    amount: uint,
    description: (string-ascii 256),
    submitted-at: uint,
    status: uint,
    processed-at: (optional uint)
  }
)

(define-map flight-delays
  { flight-number: (string-ascii 16), date: uint }
  { delay-hours: uint, compensation-rate: uint }
)

(define-map emergency-assistance
  { assistance-id: uint }
  {
    policy-id: uint,
    type: (string-ascii 64),
    location: (string-ascii 128),
    requested-at: uint,
    status: (string-ascii 32)
  }
)

;; public functions

;; Create a new travel insurance policy
(define-public (create-policy (premium uint) (coverage-amount uint) (duration-blocks uint) 
                             (destination (string-ascii 64)) (flight-number (string-ascii 16))
                             (emergency-contact (string-ascii 128)))
  (let ((policy-id (+ (var-get policy-counter) u1))
        (start-block stacks-block-height)
        (end-block (+ stacks-block-height duration-blocks)))
    (try! (stx-transfer? premium tx-sender contract-owner))
    (map-set policies
      { policy-id: policy-id }
      {
        holder: tx-sender,
        premium: premium,
        coverage-amount: coverage-amount,
        start-block: start-block,
        end-block: end-block,
        destination: destination,
        flight-number: flight-number,
        status: policy-active,
        emergency-contact: emergency-contact
      }
    )
    (var-set policy-counter policy-id)
    (ok policy-id)
  )
)

;; Activate policy (automatic when created, but can be called to reactivate)
(define-public (activate-policy (policy-id uint))
  (let ((policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found)))
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (<= stacks-block-height (get end-block policy)) err-invalid-policy)
    (map-set policies
      { policy-id: policy-id }
      (merge policy { status: policy-active })
    )
    (ok true)
  )
)

;; Submit a claim for compensation
(define-public (submit-claim (policy-id uint) (claim-type (string-ascii 32)) 
                            (amount uint) (description (string-ascii 256)))
  (let ((policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
        (claim-id (+ (var-get claim-counter) u1)))
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (is-eq (get status policy) policy-active) err-invalid-policy)
    (asserts! (<= amount (get coverage-amount policy)) err-insufficient-coverage)
    (asserts! (<= stacks-block-height (get end-block policy)) err-claim-expired)
    (map-set claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: tx-sender,
        claim-type: claim-type,
        amount: amount,
        description: description,
        submitted-at: stacks-block-height,
        status: claim-pending,
        processed-at: none
      }
    )
    (var-set claim-counter claim-id)
    (ok claim-id)
  )
)

;; Process flight delay compensation automatically
(define-public (process-flight-delay (flight-number (string-ascii 16)) (date uint) 
                                   (delay-hours uint) (compensation-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set flight-delays
      { flight-number: flight-number, date: date }
      { delay-hours: delay-hours, compensation-rate: compensation-rate }
    )
    (ok true)
  )
)

;; Request emergency assistance
(define-public (request-emergency-assistance (policy-id uint) (assistance-type (string-ascii 64))
                                           (location (string-ascii 128)))
  (let ((policy (unwrap! (map-get? policies { policy-id: policy-id }) err-not-found))
        (assistance-id (+ (var-get claim-counter) u1)))
    (asserts! (is-eq (get holder policy) tx-sender) err-owner-only)
    (asserts! (is-eq (get status policy) policy-active) err-invalid-policy)
    (map-set emergency-assistance
      { assistance-id: assistance-id }
      {
        policy-id: policy-id,
        type: assistance-type,
        location: location,
        requested-at: stacks-block-height,
        status: "pending"
      }
    )
    (ok assistance-id)
  )
)

;; Approve and pay out claim (owner only)
(define-public (approve-claim (claim-id uint))
  (let ((claim (unwrap! (map-get? claims { claim-id: claim-id }) err-not-found))
        (policy (unwrap! (map-get? policies { policy-id: (get policy-id claim) }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status claim) claim-pending) err-claim-already-processed)
    (try! (as-contract (stx-transfer? (get amount claim) contract-owner (get claimant claim))))
    (map-set claims
      { claim-id: claim-id }
      (merge claim { status: claim-approved, processed-at: (some stacks-block-height) })
    )
    (ok true)
  )
)

;; read only functions

;; Get policy details
(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
  (map-get? claims { claim-id: claim-id })
)

;; Get flight delay information
(define-read-only (get-flight-delay (flight-number (string-ascii 16)) (date uint))
  (map-get? flight-delays { flight-number: flight-number, date: date })
)

;; Get emergency assistance request
(define-read-only (get-emergency-assistance (assistance-id uint))
  (map-get? emergency-assistance { assistance-id: assistance-id })
)

;; Check if policy is active
(define-read-only (is-policy-active (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy (and (is-eq (get status policy) policy-active)
                (<= stacks-block-height (get end-block policy)))
    false
  )
)

;; Calculate flight delay compensation
(define-read-only (calculate-delay-compensation (policy-id uint) (flight-number (string-ascii 16)) (date uint))
  (let ((policy (unwrap! (map-get? policies { policy-id: policy-id }) (err u0)))
        (delay-info (unwrap! (map-get? flight-delays { flight-number: flight-number, date: date }) (err u0))))
    (ok (* (get delay-hours delay-info) (get compensation-rate delay-info)))
  )
)

;; Get policy counter
(define-read-only (get-policy-counter)
  (var-get policy-counter)
)

;; Get claim counter
(define-read-only (get-claim-counter)
  (var-get claim-counter)
)

