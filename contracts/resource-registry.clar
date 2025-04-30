;; EcoLedger: Resource Registry Contract

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-RESOURCE-NOT-FOUND (err u404))
(define-constant ERR-INVALID-AMOUNT (err u501))
(define-constant ERR-ALLOCATION-EXCEEDED (err u502))
(define-constant ERR-TOO-SOON (err u503))

(define-constant CONTRACT-OWNER tx-sender)

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

(define-map resource-rights
  { resource-id: uint, owner: principal }
  {
    allocation-amount: uint,
    usage-reported: uint
  }
)

(define-map resource-allocations
  { resource-id: uint }
  {
    allocated: uint
  }
)

(define-public (register-resource
  (resource-id uint)
  (name (string-ascii 50))
  (resource-type (string-ascii 20))
  (total-capacity uint)
  (regeneration-rate uint))
  (begin
    (if (is-eq tx-sender CONTRACT-OWNER)
        (begin
          (map-set resources
            { resource-id: resource-id }
            {
              name: name,
              resource-type: resource-type,
              total-capacity: total-capacity,
              regeneration-rate: regeneration-rate,
              last-regeneration: block-height
            })
          (map-set resource-allocations
            { resource-id: resource-id }
            { allocated: u0 })
          (ok true))
        ERR-NOT-AUTHORIZED)))

(define-public (allocate-resource
  (resource-id uint)
  (recipient principal)
  (amount uint))
  (let (
    (resource (map-get? resources { resource-id: resource-id }))
    (allocation (default-to { allocated: u0 }
                  (map-get? resource-allocations { resource-id: resource-id })))
  )
    (if (is-none resource)
        ERR-RESOURCE-NOT-FOUND
        (let (
          (total-capacity (get total-capacity (unwrap! resource ERR-RESOURCE-NOT-FOUND)))
          (already-allocated (get allocated allocation))
          (new-total (+ already-allocated amount))
        )
          (if (> new-total total-capacity)
              ERR-ALLOCATION-EXCEEDED
              (begin
                (map-set resource-rights
                  { resource-id: resource-id, owner: recipient }
                  { allocation-amount: amount, usage-reported: u0 })
                (map-set resource-allocations
                  { resource-id: resource-id }
                  { allocated: new-total })
                (ok true)))))))
