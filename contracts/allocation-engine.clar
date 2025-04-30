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

;; Helper function - return the minimum of two values
(define-read-only (get-min (a uint) (b uint))
  (if (< a b) a b)
)
