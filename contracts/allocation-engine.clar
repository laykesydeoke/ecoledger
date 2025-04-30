;; EcoLedger: Allocation Engine Contract

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-RESOURCE-NOT-FOUND (err u404))
(define-constant ERR-INVALID-PARAMETER (err u405))
(define-constant ERR-ALLOCATION-EXCEEDED (err u502))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Resource allocation parameters
(define-map resource-allocation-params
  { resource-id: uint }
  {
    min-allocation: uint,
    max-allocation: uint,
    conservation-percent: uint  ;; 0-100 integer percentage
  }
)

;; User allocation requests
(define-map allocation-requests
  { request-id: uint }
  {
    resource-id: uint,
    requestor: principal,
    amount: uint,
    status: (string-ascii 20)  ;; "pending", "approved", "rejected"
  }
)

;; Request ID counter
(define-data-var last-request-id uint u0)
