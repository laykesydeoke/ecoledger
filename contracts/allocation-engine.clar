;; EcoLedger: Allocation Engine Contract

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-RESOURCE-NOT-FOUND (err u404))
(define-constant ERR-INVALID-PARAMETER (err u405))
(define-constant ERR-ALLOCATION-EXCEEDED (err u502))

(define-constant CONTRACT-OWNER tx-sender)

(define-map resource-allocation-params
  { resource-id: uint }
  {
    min-allocation: uint,
    max-allocation: uint,
    conservation-percent: uint
  }
)

(define-map allocation-requests
  { request-id: uint }
  {
    resource-id: uint,
    requestor: principal,
    amount: uint,
    status: (string-ascii 20)
  }
)

(define-data-var last-request-id uint u0)

(define-read-only (get-min (a uint) (b uint))
  (if (< a b) a b)
)

;; Configure resource allocation rules
(define-public (set-allocation-params
  (resource-id uint)
  (min-allocation uint)
  (max-allocation uint)
  (conservation-percent uint))
  (begin
    (if (is-eq tx-sender CONTRACT-OWNER)
        (if (or (> conservation-percent u100) (> min-allocation max-allocation))
            ERR-INVALID-PARAMETER
            (begin
              (map-set resource-allocation-params
                { resource-id: resource-id }
                {
                  min-allocation: min-allocation,
                  max-allocation: max-allocation,
                  conservation-percent: conservation-percent
                })
              (ok true)))
        ERR-NOT-AUTHORIZED)))
