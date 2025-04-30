;; EcoLedger: Resource Registry Contract

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-RESOURCE-NOT-FOUND (err u404))
(define-constant ERR-INVALID-AMOUNT (err u501))
(define-constant ERR-ALLOCATION-EXCEEDED (err u502))
(define-constant ERR-TOO-SOON (err u503))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Resource data
(define-map resources
  { resource-id: uint }
  {
    name: (string-ascii 50),
    resource-type: (string-ascii 20),
    total-capacity: uint,
    regeneration-rate: uint,
    last-regeneration: uint
  }
)

;; Resource ownership
(define-map resource-rights
  { resource-id: uint, owner: principal }
  {
    allocation-amount: uint,
    usage-reported: uint
  }
)

;; Resource allocation tracker
(define-map resource-allocations
  { resource-id: uint }
  {
    allocated: uint
  }
)
