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

(define-public (request-allocation (resource-id uint) (amount uint))
  (let (
    (params (map-get? resource-allocation-params { resource-id: resource-id }))
  )
    (if (is-none params)
        ERR-RESOURCE-NOT-FOUND
        (let (
          (data (unwrap! params ERR-RESOURCE-NOT-FOUND))
          (min (get min-allocation data))
          (max (get max-allocation data))
        )
          (if (or (< amount min) (> amount max))
              ERR-INVALID-PARAMETER
              (let (
                (req-id (+ (var-get last-request-id) u1))
              )
                (var-set last-request-id req-id)
                (map-set allocation-requests
                  { request-id: req-id }
                  {
                    resource-id: resource-id,
                    requestor: tx-sender,
                    amount: amount,
                    status: "pending"
                  })
                (ok req-id)))))))

(define-public (update-request-status (request-id uint) (new-status (string-ascii 20)))
  (begin
    (if (is-eq tx-sender CONTRACT-OWNER)
        (let (
          (req (map-get? allocation-requests { request-id: request-id }))
        )
          (if (is-none req)
              ERR-RESOURCE-NOT-FOUND
              (let (
                (data (unwrap! req ERR-RESOURCE-NOT-FOUND))
              )
                (map-set allocation-requests
                  { request-id: request-id }
                  {
                    resource-id: (get resource-id data),
                    requestor: (get requestor data),
                    amount: (get amount data),
                    status: new-status
                  })
                (ok true))))
        ERR-NOT-AUTHORIZED)))

;; Read-only views

(define-read-only (get-allocation-params (resource-id uint))
  (map-get? resource-allocation-params { resource-id: resource-id })
)

(define-read-only (get-request (request-id uint))
  (map-get? allocation-requests { request-id: request-id })
)

(define-read-only (get-last-request-id)
  (var-get last-request-id)
)
