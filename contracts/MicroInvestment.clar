;; MicroInvestment Contract

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-CONTRACT-PAUSED (err u105))
(define-constant REFERRAL-TIER-1 u1000) ;; $1000 threshold
(define-constant REFERRAL-BONUS-1 u50)  ;; 5% bonus
(define-constant REFERRAL-BONUS-2 u100) ;; 10% bonus

(define-constant ERR-DISPUTE-NOT-FOUND (err u106))
(define-constant ERR-DISPUTE-EXPIRED (err u107))
(define-constant ERR-ALREADY-VOTED (err u108))
(define-constant ERR-DISPUTE-RESOLVED (err u109))
(define-constant DISPUTE-DURATION u1008)
(define-constant MIN-VOTES-REQUIRED u3)

(define-data-var dispute-counter uint u0)

(define-map disputes
    uint
    { business: principal,
      complainant: principal,
      reason: (string-ascii 200),
      amount-disputed: uint,
      created-at: uint,
      expires-at: uint,
      status: (string-ascii 20),
      votes-for: uint,
      votes-against: uint,
      resolved: bool }
)

(define-map dispute-votes
    { dispute-id: uint, voter: principal }
    { vote: bool, voting-power: uint }
)

(define-map dispute-evidence
    uint
    { evidence-hash: (string-ascii 64),
      description: (string-ascii 300) }
)



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


(define-map timelocks
    { investor: principal, business: principal }
    { amount: uint, unlock-height: uint }
)(define-public (timelock-investment (business principal) (amount uint) (duration uint))
    (let (
        (unlock-height (+ stacks-block-height duration))
        (timelock-key { investor: tx-sender, business: business })
    )
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (ok (map-set timelocks timelock-key { amount: amount, unlock-height: unlock-height }))))
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
    { claimed: bool, amount: uint }
)

(define-public (refer-investor (referrer principal) (amount uint))
    (let (
        (ref-key { referrer: referrer, referee: tx-sender })
    )
    ;; (asserts! (not (default-to { claimed: false, amount: u0 } (map-get? referrals ref-key))) ERR-NOT-AUTHORIZED)
    (let (
        (bonus (if (>= amount REFERRAL-TIER-1)
                 (/ (* amount REFERRAL-BONUS-2) u1000)
                 (/ (* amount REFERRAL-BONUS-1) u1000)))
    )
        (try! (as-contract (stx-transfer? bonus referrer tx-sender)))
        (ok (map-set referrals ref-key { claimed: true, amount: amount })))))




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
        

(define-map auto-investments
  { investor: principal, business: principal }
  { amount: uint, frequency: uint, last-execution: uint, active: bool }
)

(define-public (set-auto-investment (business principal) (amount uint) (frequency uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> frequency u0) ERR-INVALID-AMOUNT)
    (ok (map-set auto-investments { investor: tx-sender, business: business }
      { amount: amount, frequency: frequency, last-execution: stacks-block-height, active: true }))
  )
)
(define-public (withdraw-business-funds (amount uint))
  (let (
    (business-info (get-business-info tx-sender))
    (pool-info (default-to { total-raised: u0, is-active: false } (map-get? business-pool tx-sender)))
    (emergency-current (var-get emergency-fund))
    (fee (/ (* amount EMERGENCY-FEE) u1000))
    (net-amount (- amount fee))
  )
    (asserts! (get is-active business-info) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get total-raised pool-info) amount) ERR-INSUFFICIENT-FUNDS)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? net-amount (as-contract tx-sender) tx-sender))
    (var-set emergency-fund (+ emergency-current fee))
    (map-set business-pool tx-sender
      { total-raised: (- (get total-raised pool-info) amount), is-active: true })
    (ok true)
  )
)

(define-public (raise-dispute (business principal) (reason (string-ascii 200)) (amount uint) (evidence-hash (string-ascii 64)) (evidence-desc (string-ascii 300)))
    (let (
        (dispute-id (+ (var-get dispute-counter) u1))
        (current-block stacks-block-height)
        (expiry-block (+ current-block DISPUTE-DURATION))
        (investor-data (get-investment tx-sender))
    )
        (asserts! (> (get amount investor-data) u0) ERR-NOT-AUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (var-set dispute-counter dispute-id)
        (map-set disputes dispute-id
            { business: business,
              complainant: tx-sender,
              reason: reason,
              amount-disputed: amount,
              created-at: current-block,
              expires-at: expiry-block,
              status: "active",
              votes-for: u0,
              votes-against: u0,
              resolved: false })
        (map-set dispute-evidence dispute-id
            { evidence-hash: evidence-hash,
              description: evidence-desc })
        (ok dispute-id)
    )
)

(define-public (vote-on-dispute (dispute-id uint) (vote-for bool))
    (let (
        (dispute-data (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
        (voter-investment (get-investment tx-sender))
        (voting-power (get amount voter-investment))
        (vote-key { dispute-id: dispute-id, voter: tx-sender })
        (current-block stacks-block-height)
    )
        (asserts! (not (get resolved dispute-data)) ERR-DISPUTE-RESOLVED)
        (asserts! (< current-block (get expires-at dispute-data)) ERR-DISPUTE-EXPIRED)
        (asserts! (> voting-power u0) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? dispute-votes vote-key)) ERR-ALREADY-VOTED)
        (map-set dispute-votes vote-key
            { vote: vote-for, voting-power: voting-power })
        (if vote-for
            (map-set disputes dispute-id
                (merge dispute-data { votes-for: (+ (get votes-for dispute-data) voting-power) }))
            (map-set disputes dispute-id
                (merge dispute-data { votes-against: (+ (get votes-against dispute-data) voting-power) })))
        (ok true)
    )
)

(define-public (resolve-dispute (dispute-id uint))
    (let (
        (dispute-data (unwrap! (map-get? disputes dispute-id) ERR-DISPUTE-NOT-FOUND))
        (current-block stacks-block-height)
        (total-votes (+ (get votes-for dispute-data) (get votes-against dispute-data)))
        (votes-for (get votes-for dispute-data))
        (votes-against (get votes-against dispute-data))
        (business (get business dispute-data))
        (complainant (get complainant dispute-data))
        (disputed-amount (get amount-disputed dispute-data))
    )
        (asserts! (not (get resolved dispute-data)) ERR-DISPUTE-RESOLVED)
        (asserts! (>= current-block (get expires-at dispute-data)) ERR-NOT-AUTHORIZED)
        (asserts! (>= total-votes MIN-VOTES-REQUIRED) ERR-NOT-AUTHORIZED)
        (if (> votes-for votes-against)
            (begin
                (try! (as-contract (stx-transfer? disputed-amount tx-sender complainant)))
                (map-set disputes dispute-id
                    (merge dispute-data { status: "upheld", resolved: true })))
            (map-set disputes dispute-id
                (merge dispute-data { status: "dismissed", resolved: true })))
        (ok true)
    )
)

(define-read-only (get-dispute (dispute-id uint))
    (map-get? disputes dispute-id)
)

(define-read-only (get-dispute-evidence (dispute-id uint))
    (map-get? dispute-evidence dispute-id)
)

(define-read-only (get-user-vote (dispute-id uint) (voter principal))
    (map-get? dispute-votes { dispute-id: dispute-id, voter: voter })
)

(define-read-only (get-active-disputes)
    (var-get dispute-counter)
)


(define-constant ERR-LISTING-NOT-FOUND (err u201))
(define-constant ERR-INSUFFICIENT-LISTING (err u202))
(define-constant ERR-CANNOT-BUY-OWN (err u203))
(define-constant ERR-LISTING-EXPIRED (err u204))
(define-constant LIQUIDITY-FEE u25)

(define-data-var listing-counter uint u0)

(define-map liquidity-listings
    uint
    { seller: principal,
      business: principal,
      amount: uint,
      price-per-share: uint,
      expires-at: uint,
      active: bool }
)

(define-map liquidity-providers
    principal
    { total-provided: uint,
      fees-earned: uint,
      last-reward: uint }
)

(define-public (list-investment-for-sale (business principal) (amount uint) (price-per-share uint) (duration uint))
    (let (
        (listing-id (+ (var-get listing-counter) u1))
        (investor-data (get-investment tx-sender))
        (expires-block (+ stacks-block-height duration))
    )
        (asserts! (>= (get amount investor-data) amount) ERR-INSUFFICIENT-FUNDS)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> price-per-share u0) ERR-INVALID-AMOUNT)
        (var-set listing-counter listing-id)
        (map-set liquidity-listings listing-id
            { seller: tx-sender,
              business: business,
              amount: amount,
              price-per-share: price-per-share,
              expires-at: expires-block,
              active: true })
        (ok listing-id)
    )
)

(define-public (buy-listed-investment (listing-id uint) (amount uint))
    (let (
        (listing-data (unwrap! (map-get? liquidity-listings listing-id) ERR-LISTING-NOT-FOUND))
        (seller (get seller listing-data))
        (business (get business listing-data))
        (listing-amount (get amount listing-data))
        (price-per-share (get price-per-share listing-data))
        (total-cost (* amount price-per-share))
        (fee-amount (/ (* total-cost LIQUIDITY-FEE) u1000))
        (seller-payment (- total-cost fee-amount))
        (current-block stacks-block-height)
        (buyer-investment (get-investment tx-sender))
        (seller-investment (get-investment seller))
    )
        (asserts! (get active listing-data) ERR-LISTING-NOT-FOUND)
        (asserts! (< current-block (get expires-at listing-data)) ERR-LISTING-EXPIRED)
        (asserts! (not (is-eq tx-sender seller)) ERR-CANNOT-BUY-OWN)
        (asserts! (<= amount listing-amount) ERR-INSUFFICIENT-LISTING)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
        (try! (as-contract (stx-transfer? seller-payment tx-sender seller)))
        (map-set investments tx-sender
            { amount: (+ (get amount buyer-investment) amount),
              last-investment: stacks-block-height })
        (map-set investments seller
            { amount: (- (get amount seller-investment) amount),
              last-investment: (get last-investment seller-investment) })
        (if (is-eq amount listing-amount)
            (map-set liquidity-listings listing-id
                (merge listing-data { active: false }))
            (map-set liquidity-listings listing-id
                (merge listing-data { amount: (- listing-amount amount) })))
        (ok true)
    )
)

(define-public (provide-liquidity (amount uint))
    (let (
        (provider-data (default-to { total-provided: u0, fees-earned: u0, last-reward: u0 }
            (map-get? liquidity-providers tx-sender)))
    )
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set liquidity-providers tx-sender
            { total-provided: (+ amount (get total-provided provider-data)),
              fees-earned: (get fees-earned provider-data),
              last-reward: stacks-block-height })
        (ok true)
    )
)

(define-public (claim-liquidity-rewards)
    (let (
        (provider-data (unwrap! (map-get? liquidity-providers tx-sender) ERR-NOT-AUTHORIZED))
        (total-provided (get total-provided provider-data))
        (reward-amount (/ total-provided u100))
    )
        (asserts! (> total-provided u0) ERR-NOT-AUTHORIZED)
        (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
        (map-set liquidity-providers tx-sender
            (merge provider-data { last-reward: stacks-block-height }))
        (ok reward-amount)
    )
)

(define-public (cancel-listing (listing-id uint))
    (let (
        (listing-data (unwrap! (map-get? liquidity-listings listing-id) ERR-LISTING-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get seller listing-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get active listing-data) ERR-LISTING-NOT-FOUND)
        (map-set liquidity-listings listing-id
            (merge listing-data { active: false }))
        (ok true)
    )
)

(define-read-only (get-listing (listing-id uint))
    (map-get? liquidity-listings listing-id)
)

(define-read-only (get-liquidity-provider (provider principal))
    (map-get? liquidity-providers provider)
)

(define-read-only (get-active-listings)
    (var-get listing-counter)
)
