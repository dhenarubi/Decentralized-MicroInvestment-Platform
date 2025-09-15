;; Investment Commitment Scheduler Contract
;; Enables binding investment commitments with milestone-based escrow releases

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-INVALID-AMOUNT (err u401))
(define-constant ERR-COMMITMENT-NOT-FOUND (err u402))
(define-constant ERR-INSUFFICIENT-ESCROW (err u403))
(define-constant ERR-MILESTONE-NOT-READY (err u404))
(define-constant ERR-ALREADY-VERIFIED (err u405))
(define-constant ERR-COMMITMENT-EXPIRED (err u406))
(define-constant ERR-EARLY-WITHDRAWAL (err u407))

;; Constants
(define-constant PENALTY-RATE u100) ;; 10% penalty for early withdrawal
(define-constant MIN-COMMITMENT-DURATION u1008) ;; ~1 week minimum
(define-constant VERIFICATION-THRESHOLD u3) ;; Minimum verifiers needed
(define-constant MAX-MILESTONES u10)

;; Data variables
(define-data-var commitment-counter uint u0)
(define-data-var total-escrowed uint u0)

;; Investment commitments with escrow functionality
(define-map investment-commitments
    uint
    { investor: principal,
      business: principal,
      total-commitment: uint,
      escrow-balance: uint,
      released-amount: uint,
      commitment-duration: uint,
      created-at: uint,
      expires-at: uint,
      active: bool,
      penalty-applied: bool }
)

;; Milestone-based release schedule
(define-map commitment-milestones
    { commitment-id: uint, milestone-id: uint }
    { description: (string-ascii 200),
      release-amount: uint,
      required-verifications: uint,
      current-verifications: uint,
      completed: bool,
      deadline: uint }
)

;; Milestone verification tracking
(define-map milestone-verifications
    { commitment-id: uint, milestone-id: uint, verifier: principal }
    { verified: bool,
      verification-date: uint,
      comments: (string-ascii 100) }
)

;; Community verifiers with reputation
(define-map community-verifiers
    principal
    { reputation-score: uint,
      total-verifications: uint,
      accurate-verifications: uint,
      active: bool }
)

;; Commitment performance metrics
(define-map commitment-performance
    uint
    { milestones-completed: uint,
      average-completion-time: uint,
      verifier-consensus: uint,
      performance-score: uint }
)

;; Create a new investment commitment with escrow
(define-public (create-commitment 
    (business principal)
    (total-amount uint)
    (duration uint))
    (let (
        (commitment-id (+ (var-get commitment-counter) u1))
        (expires-at (+ stacks-block-height duration))
    )
        (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= duration MIN-COMMITMENT-DURATION) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
        (var-set commitment-counter commitment-id)
        (var-set total-escrowed (+ (var-get total-escrowed) total-amount))
        (map-set investment-commitments commitment-id
            { investor: tx-sender,
              business: business,
              total-commitment: total-amount,
              escrow-balance: total-amount,
              released-amount: u0,
              commitment-duration: duration,
              created-at: stacks-block-height,
              expires-at: expires-at,
              active: true,
              penalty-applied: false })
        (ok commitment-id)
    )
)

;; Add milestone to commitment with release amount
(define-public (add-commitment-milestone
    (commitment-id uint)
    (milestone-id uint)
    (description (string-ascii 200))
    (release-amount uint)
    (deadline uint))
    (let (
        (commitment-data (unwrap! (map-get? investment-commitments commitment-id) ERR-COMMITMENT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get business commitment-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get active commitment-data) ERR-COMMITMENT-NOT-FOUND)
        (asserts! (<= release-amount (get escrow-balance commitment-data)) ERR-INSUFFICIENT-ESCROW)
        (asserts! (> deadline stacks-block-height) ERR-INVALID-AMOUNT)
        (map-set commitment-milestones { commitment-id: commitment-id, milestone-id: milestone-id }
            { description: description,
              release-amount: release-amount,
              required-verifications: VERIFICATION-THRESHOLD,
              current-verifications: u0,
              completed: false,
              deadline: deadline })
        (ok true)
    )
)

;; Register as community verifier
(define-public (register-verifier)
    (begin
        (map-set community-verifiers tx-sender
            { reputation-score: u100,
              total-verifications: u0,
              accurate-verifications: u0,
              active: true })
        (ok true)
    )
)

;; Verify milestone completion
(define-public (verify-milestone
    (commitment-id uint)
    (milestone-id uint)
    (comments (string-ascii 100)))
    (let (
        (milestone-key { commitment-id: commitment-id, milestone-id: milestone-id })
        (verifier-key { commitment-id: commitment-id, milestone-id: milestone-id, verifier: tx-sender })
        (milestone-data (unwrap! (map-get? commitment-milestones milestone-key) ERR-MILESTONE-NOT-READY))
        (verifier-data (unwrap! (map-get? community-verifiers tx-sender) ERR-NOT-AUTHORIZED))
    )
        (asserts! (get active verifier-data) ERR-NOT-AUTHORIZED)
        (asserts! (not (get completed milestone-data)) ERR-ALREADY-VERIFIED)
        (asserts! (is-none (map-get? milestone-verifications verifier-key)) ERR-ALREADY-VERIFIED)
        (asserts! (<= stacks-block-height (get deadline milestone-data)) ERR-MILESTONE-NOT-READY)
        (map-set milestone-verifications verifier-key
            { verified: true,
              verification-date: stacks-block-height,
              comments: comments })
        (let (
            (new-verifications (+ (get current-verifications milestone-data) u1))
        )
            (map-set commitment-milestones milestone-key
                (merge milestone-data { current-verifications: new-verifications }))
            (map-set community-verifiers tx-sender
                (merge verifier-data { total-verifications: (+ (get total-verifications verifier-data) u1) }))
            (if (>= new-verifications (get required-verifications milestone-data))
                (complete-milestone commitment-id milestone-id)
                (ok true))
        )
    )
)

;; Complete milestone and release escrow funds
(define-private (complete-milestone (commitment-id uint) (milestone-id uint))
    (let (
        (milestone-key { commitment-id: commitment-id, milestone-id: milestone-id })
        (milestone-data (unwrap! (map-get? commitment-milestones milestone-key) ERR-MILESTONE-NOT-READY))
        (commitment-data (unwrap! (map-get? investment-commitments commitment-id) ERR-COMMITMENT-NOT-FOUND))
        (release-amount (get release-amount milestone-data))
        (business (get business commitment-data))
    )
        (asserts! (not (get completed milestone-data)) ERR-ALREADY-VERIFIED)
        (asserts! (>= (get escrow-balance commitment-data) release-amount) ERR-INSUFFICIENT-ESCROW)
        (try! (as-contract (stx-transfer? release-amount tx-sender business)))
        (map-set commitment-milestones milestone-key
            (merge milestone-data { completed: true }))
        (map-set investment-commitments commitment-id
            (merge commitment-data { 
                escrow-balance: (- (get escrow-balance commitment-data) release-amount),
                released-amount: (+ (get released-amount commitment-data) release-amount) }))
        (var-set total-escrowed (- (var-get total-escrowed) release-amount))
        (ok true)
    )
)

;; Early withdrawal with penalty
(define-public (withdraw-commitment (commitment-id uint))
    (let (
        (commitment-data (unwrap! (map-get? investment-commitments commitment-id) ERR-COMMITMENT-NOT-FOUND))
        (remaining-balance (get escrow-balance commitment-data))
        (penalty-amount (/ (* remaining-balance PENALTY-RATE) u1000))
        (withdrawal-amount (- remaining-balance penalty-amount))
    )
        (asserts! (is-eq tx-sender (get investor commitment-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get active commitment-data) ERR-COMMITMENT-NOT-FOUND)
        (asserts! (> remaining-balance u0) ERR-INSUFFICIENT-ESCROW)
        (asserts! (< stacks-block-height (get expires-at commitment-data)) ERR-EARLY-WITHDRAWAL)
        (try! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)))
        (map-set investment-commitments commitment-id
            (merge commitment-data { 
                escrow-balance: u0,
                active: false,
                penalty-applied: true }))
        (var-set total-escrowed (- (var-get total-escrowed) remaining-balance))
        (ok withdrawal-amount)
    )
)

;; Update verifier reputation based on accuracy
(define-public (update-verifier-reputation (verifier principal) (accurate bool))
    (let (
        (verifier-data (unwrap! (map-get? community-verifiers verifier) ERR-NOT-AUTHORIZED))
        (current-score (get reputation-score verifier-data))
        (new-score (if accurate 
                      (+ current-score u10) 
                      (if (> current-score u10) (- current-score u10) u0)))
    )
        (map-set community-verifiers verifier
            (merge verifier-data { 
                reputation-score: new-score,
                accurate-verifications: (if accurate 
                                          (+ (get accurate-verifications verifier-data) u1)
                                          (get accurate-verifications verifier-data)) }))
        (ok new-score)
    )
)

;; Read-only functions
(define-read-only (get-commitment (commitment-id uint))
    (map-get? investment-commitments commitment-id)
)

(define-read-only (get-commitment-milestone (commitment-id uint) (milestone-id uint))
    (map-get? commitment-milestones { commitment-id: commitment-id, milestone-id: milestone-id })
)

(define-read-only (get-verifier (verifier principal))
    (map-get? community-verifiers verifier)
)

(define-read-only (get-milestone-verification (commitment-id uint) (milestone-id uint) (verifier principal))
    (map-get? milestone-verifications { commitment-id: commitment-id, milestone-id: milestone-id, verifier: verifier })
)

(define-read-only (get-total-escrowed)
    (var-get total-escrowed)
)

(define-read-only (get-commitment-count)
    (var-get commitment-counter)
)

(define-read-only (calculate-commitment-progress (commitment-id uint))
    (let (
        (commitment-data (unwrap! (map-get? investment-commitments commitment-id) (err u0)))
        (total-commitment (get total-commitment commitment-data))
        (released-amount (get released-amount commitment-data))
    )
        (if (> total-commitment u0)
            (ok (/ (* released-amount u100) total-commitment))
            (ok u0))
    )
)
