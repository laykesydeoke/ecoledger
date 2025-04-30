;; EcoLedger: Resource Registry Contract

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-RESOURCE-NOT-FOUND (err u404))
(define-constant ERR-INVALID-AMOUNT (err u501))
(define-constant ERR-ALLOCATION-EXCEEDED (err u502))
(define-constant ERR-TOO-SOON (err u503))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Resource metadata
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

;; Ownership and usage record
(define-map resource-rights
  { resource-id: uint, owner: principal }
  {
    allocation-amount: uint,
    usage-reported: uint
  }
)

;; Total allocations record
(define-map resource-allocations
  { resource-id: uint }
  {
    allocated: uint
  }
)

;; Register a new resource
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

;; Allocate a portion of a resource to a recipient
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

;; Report usage by resource owner
(define-public (report-usage
  (resource-id uint)
  (amount-used uint))
  (let (
    (rights (map-get? resource-rights { resource-id: resource-id, owner: tx-sender }))
  )
    (if (is-none rights)
        ERR-NOT-AUTHORIZED
        (let (
          (data (unwrap! rights ERR-NOT-AUTHORIZED))
          (current-usage (get usage-reported data))
          (new-usage (+ current-usage amount-used))
        )
          (map-set resource-rights
            { resource-id: resource-id, owner: tx-sender }
            {
              allocation-amount: (get allocation-amount data),
              usage-reported: new-usage
            })
          (ok true)))))

;; Regenerate available resources based on time passed
(define-public (regenerate-resource (resource-id uint))
  (let (
    (resource (map-get? resources { resource-id: resource-id }))
  )
    (if (is-none resource)
        ERR-RESOURCE-NOT-FOUND
        (let (
          (data (unwrap! resource ERR-RESOURCE-NOT-FOUND))
          (last (get last-regeneration data))
          (rate (get regeneration-rate data))
          (total (get total-capacity data))
          (allocated (default-to { allocated: u0 }
                        (map-get? resource-allocations { resource-id: resource-id })))
        )
          (if (<= block-height last)
              ERR-TOO-SOON
              (let (
                (new-allocated (max u0 (- (get allocated allocated) rate)))
              )
                (map-set resource-allocations
                  { resource-id: resource-id }
                  { allocated: new-allocated })
                (map-set resources
                  { resource-id: resource-id }
                  {
                    name: (get name data),
                    resource-type: (get resource-type data),
                    total-capacity: total,
                    regeneration-rate: rate,
                    last-regeneration: block-height
                  })
                (ok true)))))))

;; View: Get full resource data
(define-read-only (get-resource (resource-id uint))
  (map-get? resources { resource-id: resource-id })
)

;; View: Get total allocation for resource
(define-read-only (get-allocation (resource-id uint))
  (map-get? resource-allocations { resource-id: resource-id })
)

;; View: Get user-specific rights
(define-read-only (get-user-rights (resource-id uint) (user principal))
  (map-get? resource-rights { resource-id: resource-id, owner: user })
)
