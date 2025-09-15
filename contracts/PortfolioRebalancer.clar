;; Portfolio Rebalancer Contract
;; Enables automated portfolio rebalancing based on business performance and market conditions

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-INVALID-AMOUNT (err u301))
(define-constant ERR-STRATEGY-NOT-FOUND (err u302))
(define-constant ERR-INSUFFICIENT-FUNDS (err u303))
(define-constant ERR-REBALANCE-TOO-SOON (err u304))
(define-constant ERR-THRESHOLD-NOT-MET (err u305))
(define-constant ERR-BUSINESS-NOT-FOUND (err u306))
(define-constant ERR-INVALID-PERCENTAGE (err u307))

;; Rebalancing parameters
(define-constant MIN-REBALANCE-INTERVAL u144) ;; ~24 hours in blocks
(define-constant MAX-ALLOCATION-PERCENTAGE u500) ;; 50% max per business
(define-constant REBALANCE-FEE u10) ;; 1% fee
(define-constant PERFORMANCE-THRESHOLD u150) ;; 15% performance threshold

;; Data Variables
(define-data-var strategy-counter uint u0)
(define-data-var total-rebalances uint u0)

;; Rebalancing strategies that investors can create
(define-map rebalancing-strategies
    uint
    { investor: principal,
      name: (string-ascii 50),
      target-businesses: (list 10 principal),
      target-allocations: (list 10 uint),
      performance-weights: (list 10 uint),
      min-threshold: uint,
      max-allocation: uint,
      auto-enabled: bool,
      created-at: uint,
      last-rebalance: uint }
)

;; Track business performance metrics for rebalancing decisions
(define-map business-performance
    principal
    { revenue-growth: uint,
      roi-percentage: uint,
      risk-score: uint,
      market-cap: uint,
      last-updated: uint }
)

;; Portfolio allocations for each investor
(define-map portfolio-allocations
    { investor: principal, business: principal }
    { allocated-amount: uint,
      target-percentage: uint,
      current-percentage: uint,
      last-rebalance: uint }
)

;; Rebalancing triggers and conditions
(define-map rebalance-triggers
    principal
    { deviation-threshold: uint,
      time-threshold: uint,
      performance-trigger: bool,
      market-trigger: bool,
      manual-only: bool }
)

;; Historical rebalancing data
(define-map rebalance-history
    { investor: principal, timestamp: uint }
    { old-allocations: (list 10 uint),
      new-allocations: (list 10 uint),
      businesses: (list 10 principal),
      total-value: uint,
      gas-used: uint }
)

;; Market sentiment and volatility tracking
(define-map market-conditions
    uint
    { volatility-index: uint,
      sentiment-score: uint,
      liquidity-ratio: uint,
      block-height: uint }
)

;; Create a new rebalancing strategy
(define-public (create-rebalancing-strategy 
    (name (string-ascii 50))
    (target-businesses (list 10 principal))
    (target-allocations (list 10 uint))
    (performance-weights (list 10 uint))
    (min-threshold uint)
    (max-allocation uint))
    (let (
        (strategy-id (+ (var-get strategy-counter) u1))
        (total-allocation (fold + target-allocations u0))
    )
        (asserts! (is-eq (len target-businesses) (len target-allocations)) ERR-INVALID-AMOUNT)
        (asserts! (is-eq (len target-allocations) (len performance-weights)) ERR-INVALID-AMOUNT)
        (asserts! (is-eq total-allocation u1000) ERR-INVALID-PERCENTAGE) ;; Must equal 100%
        (asserts! (<= max-allocation MAX-ALLOCATION-PERCENTAGE) ERR-INVALID-PERCENTAGE)
        (var-set strategy-counter strategy-id)
        (map-set rebalancing-strategies strategy-id
            { investor: tx-sender,
              name: name,
              target-businesses: target-businesses,
              target-allocations: target-allocations,
              performance-weights: performance-weights,
              min-threshold: min-threshold,
              max-allocation: max-allocation,
              auto-enabled: true,
              created-at: stacks-block-height,
              last-rebalance: u0 })
        (ok strategy-id)
    )
)

;; Update business performance metrics
(define-public (update-business-performance 
    (business principal)
    (revenue-growth uint)
    (roi-percentage uint)
    (risk-score uint)
    (market-cap uint))
    (begin
        (asserts! (<= risk-score u1000) ERR-INVALID-AMOUNT) ;; Risk score max 100%
        (asserts! (<= roi-percentage u2000) ERR-INVALID-AMOUNT) ;; ROI max 200%
        (map-set business-performance business
            { revenue-growth: revenue-growth,
              roi-percentage: roi-percentage,
              risk-score: risk-score,
              market-cap: market-cap,
              last-updated: stacks-block-height })
        (ok true)
    )
)

;; Set rebalancing triggers for an investor
(define-public (set-rebalance-triggers
    (deviation-threshold uint)
    (time-threshold uint)
    (performance-trigger bool)
    (market-trigger bool)
    (manual-only bool))
    (begin
        (asserts! (<= deviation-threshold u500) ERR-INVALID-PERCENTAGE) ;; Max 50% deviation
        (asserts! (>= time-threshold MIN-REBALANCE-INTERVAL) ERR-INVALID-AMOUNT)
        (map-set rebalance-triggers tx-sender
            { deviation-threshold: deviation-threshold,
              time-threshold: time-threshold,
              performance-trigger: performance-trigger,
              market-trigger: market-trigger,
              manual-only: manual-only })
        (ok true)
    )
)

;; Calculate optimal allocation based on performance
(define-private (calculate-performance-allocation (performance-data (tuple (revenue-growth uint) (roi-percentage uint) (risk-score uint))) (base-allocation uint))
    (let (
        (performance-score (+ (get revenue-growth performance-data) (get roi-percentage performance-data)))
        (risk-adjusted-score (if (> (get risk-score performance-data) u500)
                               (/ performance-score u2)
                               performance-score))
        (adjustment-factor (/ risk-adjusted-score u100))
    )
        (if (< (+ base-allocation adjustment-factor) MAX-ALLOCATION-PERCENTAGE)
            (+ base-allocation adjustment-factor)
            MAX-ALLOCATION-PERCENTAGE)
    )
)

;; Check if rebalancing is needed
(define-private (should-rebalance (investor principal) (strategy-id uint))
    (let (
        (strategy-data (unwrap! (map-get? rebalancing-strategies strategy-id) false))
        (triggers (default-to 
            { deviation-threshold: u100, time-threshold: MIN-REBALANCE-INTERVAL, 
              performance-trigger: true, market-trigger: false, manual-only: false }
            (map-get? rebalance-triggers investor)))
        (last-rebalance (get last-rebalance strategy-data))
        (time-elapsed (- stacks-block-height last-rebalance))
    )
        (and
            (get auto-enabled strategy-data)
            (not (get manual-only triggers))
            (>= time-elapsed (get time-threshold triggers))
            (is-eq (get investor strategy-data) investor)
        )
    )
)

;; Execute portfolio rebalancing
(define-public (execute-rebalance (strategy-id uint))
    (let (
        (strategy-data (unwrap! (map-get? rebalancing-strategies strategy-id) ERR-STRATEGY-NOT-FOUND))
        (investor (get investor strategy-data))
        (target-businesses (get target-businesses strategy-data))
        (target-allocations (get target-allocations strategy-data))
        (rebalance-count (var-get total-rebalances))
    )
        (asserts! (is-eq tx-sender investor) ERR-NOT-AUTHORIZED)
        (asserts! (should-rebalance investor strategy-id) ERR-REBALANCE-TOO-SOON)
        (try! (process-rebalancing investor target-businesses target-allocations))
        (map-set rebalancing-strategies strategy-id
            (merge strategy-data { last-rebalance: stacks-block-height }))
        (var-set total-rebalances (+ rebalance-count u1))
        (ok true)
    )
)

;; Process the actual rebalancing logic
(define-private (process-rebalancing (investor principal) (businesses (list 10 principal)) (allocations (list 10 uint)))
    (let (
        (total-investment (get-total-investment investor))
        (rebalance-fee (/ (* total-investment REBALANCE-FEE) u1000))
        (net-amount (- total-investment rebalance-fee))
    )
        (begin
            (asserts! (> total-investment u0) ERR-INSUFFICIENT-FUNDS)
            (unwrap-panic (redistribute-investments investor businesses allocations net-amount))
            (map-set rebalance-history { investor: investor, timestamp: stacks-block-height }
                { old-allocations: (list),
                  new-allocations: allocations,
                  businesses: businesses,
                  total-value: net-amount,
                  gas-used: u1000 })
            (ok true)
        )
    )
)

;; Redistribute investments according to new allocations
(define-private (redistribute-investments (investor principal) (businesses (list 10 principal)) (allocations (list 10 uint)) (total-amount uint))
    (begin
        ;; For simplicity, just mark the rebalancing as done without complex folding
        (ok true)
    )
)

;; Get total investment amount for an investor
(define-private (get-total-investment (investor principal))
    ;; This would integrate with the main MicroInvestment contract
    ;; For now, returning a placeholder
    u10000
)

;; Update market conditions
(define-public (update-market-conditions (volatility uint) (sentiment uint) (liquidity uint))
    (let (
        (current-block stacks-block-height)
    )
        (asserts! (<= volatility u1000) ERR-INVALID-AMOUNT)
        (asserts! (<= sentiment u1000) ERR-INVALID-AMOUNT)
        (asserts! (<= liquidity u1000) ERR-INVALID-AMOUNT)
        (map-set market-conditions current-block
            { volatility-index: volatility,
              sentiment-score: sentiment,
              liquidity-ratio: liquidity,
              block-height: current-block })
        (ok true)
    )
)

;; Manual rebalancing trigger
(define-public (manual-rebalance (strategy-id uint))
    (let (
        (strategy-data (unwrap! (map-get? rebalancing-strategies strategy-id) ERR-STRATEGY-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get investor strategy-data)) ERR-NOT-AUTHORIZED)
        (try! (execute-rebalance strategy-id))
        (ok true)
    )
)

;; Toggle auto-rebalancing for a strategy
(define-public (toggle-auto-rebalancing (strategy-id uint))
    (let (
        (strategy-data (unwrap! (map-get? rebalancing-strategies strategy-id) ERR-STRATEGY-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get investor strategy-data)) ERR-NOT-AUTHORIZED)
        (map-set rebalancing-strategies strategy-id
            (merge strategy-data { auto-enabled: (not (get auto-enabled strategy-data)) }))
        (ok true)
    )
)

;; Read-only functions

(define-read-only (get-rebalancing-strategy (strategy-id uint))
    (map-get? rebalancing-strategies strategy-id)
)

(define-read-only (get-business-performance (business principal))
    (map-get? business-performance business)
)

(define-read-only (get-portfolio-allocation (investor principal) (business principal))
    (map-get? portfolio-allocations { investor: investor, business: business })
)

(define-read-only (get-rebalance-triggers (investor principal))
    (map-get? rebalance-triggers investor)
)

(define-read-only (get-rebalance-history (investor principal) (timestamp uint))
    (map-get? rebalance-history { investor: investor, timestamp: timestamp })
)

(define-read-only (get-market-conditions (target-block uint))
    (map-get? market-conditions target-block)
)

(define-read-only (get-strategy-count)
    (var-get strategy-counter)
)

(define-read-only (get-total-rebalances)
    (var-get total-rebalances)
)

;; Calculate rebalancing efficiency score
(define-read-only (calculate-efficiency-score (investor principal) (strategy-id uint))
    (let (
        (strategy-data (unwrap! (map-get? rebalancing-strategies strategy-id) (err u0)))
        (last-rebalance (get last-rebalance strategy-data))
        (time-since-rebalance (- stacks-block-height last-rebalance))
    )
        (if (> time-since-rebalance u0)
            (ok (/ u100000 time-since-rebalance))
            (ok u0)
        )
    )
)

;; Helper function for simple calculations
(define-private (calculate-allocation-amount (total-amount uint) (percentage uint))
    (/ (* total-amount percentage) u1000)
)


