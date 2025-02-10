;; MicroInvestment Contract

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))

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
