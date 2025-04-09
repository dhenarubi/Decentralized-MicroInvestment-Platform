;; MicroInvestment Contract

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-CONTRACT-PAUSED (err u105))

;; Data Maps
(define-map investments 
    principal 
    { amount: uint, 
      last-investment: uint }
)

(define-map business-pool 
    principal 
    { total-raised: uint, 
      is-active: bool }
)

;; Public Functions
(define-public (invest (amount uint) (business principal))
    (let
        ((current-balance (default-to { amount: u0, last-investment: u0 } 
            (map-get? investments tx-sender))))
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (ok (map-set investments tx-sender
            { amount: (+ amount (get amount current-balance)),
              last-investment: stacks-block-height }))))

(define-public (register-business)
    (ok (map-set business-pool tx-sender
        { total-raised: u0,
          is-active: true })))

;; Read Only Functions
(define-read-only (get-investment (investor principal))
    (default-to { amount: u0, last-investment: u0 }
        (map-get? investments investor)))

(define-read-only (get-business-info (business principal))
    (default-to { total-raised: u0, is-active: false }
        (map-get? business-pool business)))



(define-constant ERR-NO-INVESTMENT (err u103))
(define-constant WITHDRAWAL-COOLDOWN u144) ;; ~24 hours in blocks

(define-public (withdraw-investment (amount uint))
    (let (
        (current-investment (get-investment tx-sender))
        (current-amount (get amount current-investment))
        (last-investment-block (get last-investment current-investment))
    )
    (asserts! (>= current-amount amount) ERR-INSUFFICIENT-FUNDS)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= (- stacks-block-height last-investment-block) WITHDRAWAL-COOLDOWN) ERR-NOT-AUTHORIZED)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (ok (map-set investments tx-sender
        { amount: (- current-amount amount),
          last-investment: last-investment-block }))))



(define-map business-profiles
    principal
    { name: (string-ascii 50),
      description: (string-ascii 500),
      target-amount: uint }
)

(define-public (set-business-profile (name (string-ascii 50)) (description (string-ascii 500)) (target uint))
    (let ((business (get-business-info tx-sender)))
        (asserts! (get is-active business) ERR-NOT-AUTHORIZED)
        (ok (map-set business-profiles tx-sender
            { name: name,
              description: description,
              target-amount: target }))))


(define-map investor-tiers
    uint
    { min-amount: uint,
      name: (string-ascii 20),
      benefits: (string-ascii 100) }
)

(define-public (create-tier (tier-id uint) (min-amount uint) (name (string-ascii 20)) (benefits (string-ascii 100)))
    (ok (map-set investor-tiers tier-id
        { min-amount: min-amount,
          name: name,
          benefits: benefits })))


(define-constant ERR-DISTRIBUTION-FAILED (err u104))

(define-public (distribute-returns (amount uint))
    (let ((business (get-business-info tx-sender)))
        (asserts! (get is-active business) ERR-NOT-AUTHORIZED)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (ok true)))


(define-map business-milestones
    principal
    { milestones: (list 10 (string-ascii 100)),
      completed: (list 10 uint) }
)

(define-public (add-milestone (milestone (string-ascii 100)))
    (let ((current-milestones (default-to { milestones: (list), completed: (list) }
            (map-get? business-milestones tx-sender))))
        (if (< (len (get milestones current-milestones)) u10)
            (ok (map-set business-milestones tx-sender
                { milestones: (unwrap! (as-max-len? (append (get milestones current-milestones) milestone) u10) ERR-NOT-AUTHORIZED),
                  completed: (get completed current-milestones) }))
            ERR-NOT-AUTHORIZED)))


    (define-data-var contract-paused bool false)
(define-data-var contract-owner principal tx-sender)

(define-public (toggle-pause)
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (var-set contract-paused (not (var-get contract-paused))))))

(define-private (check-not-paused)
    (ok (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)))


;; Add to data maps section
(define-map investor-analytics
    principal 
    { total-invested: uint,
      businesses-backed: uint,
      last-activity: uint }
)

(define-public (update-analytics (amount uint))
    (let (
        (current-stats (default-to { total-invested: u0, businesses-backed: u1, last-activity: u0 }
            (map-get? investor-analytics tx-sender)))
    )
    (ok (map-set investor-analytics tx-sender
        { total-invested: (+ amount (get total-invested current-stats)),
          businesses-backed: (get businesses-backed current-stats),
          last-activity: stacks-block-height }))))



(define-map business-ratings
    principal
    { total-score: uint,
      raters-count: uint,
      average-rating: uint }
)

(define-public (rate-business (business principal) (score uint))
    (let (
        (current-rating (default-to { total-score: u0, raters-count: u0, average-rating: u0 }
            (map-get? business-ratings business)))
    )
    (asserts! (<= score u5) ERR-INVALID-AMOUNT)
    (ok (map-set business-ratings business
        { total-score: (+ score (get total-score current-rating)),
          raters-count: (+ u1 (get raters-count current-rating)),
          average-rating: (/ (+ score (get total-score current-rating)) (+ u1 (get raters-count current-rating))) }))))



(define-constant REFERRAL-BONUS u50) ;; 5% bonus

(define-map referrals
    { referrer: principal, referee: principal }
    { claimed: bool }
)

(define-public (refer-investor (referrer principal))
    (let (
        (ref-key { referrer: referrer, referee: tx-sender })
    )
    (asserts! (not (default-to false (get claimed (map-get? referrals ref-key)))) ERR-NOT-AUTHORIZED)
    (ok (map-set referrals ref-key { claimed: true }))))



(define-map milestone-rewards
    { business: principal, milestone-id: uint }
    { reward-amount: uint,
      claimed: bool }
)

(define-public (set-milestone-reward (milestone-id uint) (reward uint))
    (let (
        (milestone-key { business: tx-sender, milestone-id: milestone-id })
    )
    (ok (map-set milestone-rewards milestone-key
        { reward-amount: reward,
          claimed: false }))))


(define-data-var emergency-fund uint u0)
(define-constant EMERGENCY-FEE u10) ;; 1% fee

(define-public (contribute-emergency-fund (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (ok (var-set emergency-fund (+ amount (var-get emergency-fund))))))


(define-map insurance-pool
    principal
    { coverage-amount: uint,
      premium-paid: uint,
      active: bool }
)

(define-public (purchase-insurance (coverage uint))
    (let (
        (premium (/ coverage u20)) ;; 5% premium
    )
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (ok (map-set insurance-pool tx-sender
        { coverage-amount: coverage,
          premium-paid: premium,
          active: true }))))


(define-map business-metrics
    principal
    { revenue: uint,
      profit-margin: uint,
      update-frequency: uint }
)

(define-public (update-business-metrics (revenue uint) (profit-margin uint))
    (ok (map-set business-metrics tx-sender
        { revenue: revenue,
          profit-margin: profit-margin,
          update-frequency: stacks-block-height })))


(define-map investment-schedules
    principal
    { amount: uint,
      frequency: uint,
      last-execution: uint,
      active: bool }
)

(define-public (set-investment-schedule (amount uint) (frequency uint))
    (ok (map-set investment-schedules tx-sender
        { amount: amount,
          frequency: frequency,
          last-execution: stacks-block-height,
          active: true })))


(define-constant MAX-INVESTMENT-PERCENTAGE u700) ;; 70% of total investments

(define-map portfolio-limits
    principal 
    { max-per-business: uint }
)

(define-public (set-portfolio-limit (percentage uint))
    (begin
        (asserts! (<= percentage MAX-INVESTMENT-PERCENTAGE) ERR-INVALID-AMOUNT)
        (ok (map-set portfolio-limits tx-sender
            { max-per-business: percentage }))))

(define-read-only (check-portfolio-limit (investor principal) (amount uint) (business principal))
    (let (
        (limit (default-to { max-per-business: MAX-INVESTMENT-PERCENTAGE } 
            (map-get? portfolio-limits investor)))
        (total-invested (get total-invested 
            (default-to { total-invested: u0, businesses-backed: u0, last-activity: u0 }
                (map-get? investor-analytics investor))))
    )
    (ok (<= (* amount u1000) (* total-invested (get max-per-business limit))))))




(define-map revenue-sharing
    principal
    { total-shares: uint,
      unclaimed-revenue: uint,
      share-price: uint }
)

(define-public (distribute-revenue)
    (let (
        (business-data (default-to { total-shares: u0, unclaimed-revenue: u0, share-price: u0 }
            (map-get? revenue-sharing tx-sender)))
        (amount (stx-get-balance tx-sender))
    )
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (ok (map-set revenue-sharing tx-sender
            { total-shares: (get total-shares business-data),
              unclaimed-revenue: (+ amount (get unclaimed-revenue business-data)),
              share-price: (/ amount (get total-shares business-data)) })))))

(define-public (claim-revenue-share (business principal))
    (let (
        (investor-amount (get amount (get-investment tx-sender)))
        (business-data (default-to { total-shares: u0, unclaimed-revenue: u0, share-price: u0 }
            (map-get? revenue-sharing business)))
        (share-amount (* investor-amount (get share-price business-data)))
    )
    (begin
        (try! (as-contract (stx-transfer? share-amount tx-sender tx-sender)))
        (ok (map-set revenue-sharing business
            { total-shares: (get total-shares business-data),
              unclaimed-revenue: (- (get unclaimed-revenue business-data) share-amount),
              share-price: (get share-price business-data) })))))