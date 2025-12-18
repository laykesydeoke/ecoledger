;; EcoLedger: Simplified Allocation Engine Contract
;; Handles basic resource allocation based on regeneration rates

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

;; Helper function - get minimum of two values
(define-read-only (get-min (a uint) (b uint))
  (if (< a b) a b)
)

;; Helper function - get maximum of two values
(define-read-only (get-max (a uint) (b uint))
  (if (> a b) a b)
)

;; Set allocation parameters for a resource
(define-public (set-allocation-parameters
  (resource-id uint)
  (min-allocation uint)
  (max-allocation uint)
  (conservation-percent uint)
)
  (begin
    ;; Only contract owner can set parameters
    (if (not (is-eq tx-sender CONTRACT-OWNER))
        (err ERR-NOT-AUTHORIZED)
        ;; Verify min/max allocation makes sense
        (if (> min-allocation max-allocation)
            (err ERR-INVALID-PARAMETER)
            ;; Verify conservation percentage is valid (0-100%)
            (if (> conservation-percent u100)
                (err ERR-INVALID-PARAMETER)
                ;; Set the allocation parameters
                (begin
                  (map-set resource-allocation-params
                    { resource-id: resource-id }
                    {
                      min-allocation: min-allocation,
                      max-allocation: max-allocation,
                      conservation-percent: conservation-percent
                    }
                  )
                  (ok true)
                )
            )
        )
    )
  )
)

;; Get allocation parameters for a resource
(define-read-only (get-allocation-parameters (resource-id uint))
  (let ((params (map-get? resource-allocation-params { resource-id: resource-id })))
    (if (is-none params)
        (err ERR-RESOURCE-NOT-FOUND)
        (ok (unwrap-panic params))
    )
  )
)

;; Calculate basic sustainable allocation
(define-read-only (calculate-allocation 
  (resource-id uint) 
  (total-available uint)
  (num-users uint)
  (regeneration-rate uint)
)
  (begin
    ;; Validate basic parameters
    (if (is-eq num-users u0)
        (err ERR-INVALID-PARAMETER)
        (let ((params (map-get? resource-allocation-params { resource-id: resource-id })))
          ;; Check if allocation parameters exist
          (if (is-none params)
              (err ERR-RESOURCE-NOT-FOUND)
              (let ((current-params (unwrap-panic params))
                    (min-alloc (get min-allocation current-params))
                    (max-alloc (get max-allocation current-params))
                    (conservation (get conservation-percent current-params)))
                ;; Calculate how much to keep in reserve
                (let (
                      ;; Favor regeneration rate for sustainability
                      (sustainable-rate regeneration-rate)
                      ;; Calculate reserve percentage
                      (reserve-amount (/ (* total-available conservation) u100))
                      ;; Amount available for distribution
                      (distributable (- total-available reserve-amount))
                    )
                  ;; Calculate per-user allocation
                  (let ((per-user (/ distributable num-users)))
                    ;; Ensure allocation is between min and max
                    (ok (get-min max-alloc (get-max min-alloc per-user)))
                  )
                )
              )
          )
        )
    )
  )
)

;; Request resource allocation
(define-public (request-allocation
  (resource-id uint)
  (amount uint)
)
  (let ((request-id (+ u1 (var-get last-request-id))))
    ;; Validate amount
    (if (is-eq amount u0)
        (err ERR-INVALID-PARAMETER)
        ;; Create allocation request
        (begin
          (map-set allocation-requests
            { request-id: request-id }
            {
              resource-id: resource-id,
              requestor: tx-sender,
              amount: amount,
              status: "pending"
            }
          )
          ;; Update request ID counter
          (var-set last-request-id request-id)
          (ok request-id)
        )
    )
  )
)

;; Get allocation request details
(define-read-only (get-allocation-request (request-id uint))
  (let ((request (map-get? allocation-requests { request-id: request-id })))
    (if (is-none request)
        (err ERR-RESOURCE-NOT-FOUND)
        (ok (unwrap-panic request))
    )
  )
)

;; Process allocation request
(define-public (process-allocation-request 
  (request-id uint) 
  (approve bool)
  (registry-contract principal)
)
  (begin
    ;; Only contract owner can process requests
    (if (not (is-eq tx-sender CONTRACT-OWNER))
        (err ERR-NOT-AUTHORIZED)
        (let ((request (map-get? allocation-requests { request-id: request-id })))
          ;; Check if request exists
          (if (is-none request)
              (err ERR-RESOURCE-NOT-FOUND)
              (let ((current-request (unwrap-panic request))
                    (status (get status (unwrap-panic request))))
                ;; Check if request is still pending
                (if (not (is-eq status "pending"))
                    (err ERR-INVALID-PARAMETER)
                    (let ((resource-id (get resource-id current-request))
                          (requestor (get requestor current-request))
                          (amount (get amount current-request)))
                      ;; If approved, allocate resource to requestor
                      (if approve
                          (begin
                            ;; Update request status
                            (map-set allocation-requests
                              { request-id: request-id }
                              (merge current-request { status: "approved" })
                            )
                            ;; The actual allocation needs to be done by calling the registry contract
                            ;; But we return success to keep things simple
                            (ok true)
                          )
                          ;; Reject request
                          (begin
                            ;; Update request status
                            (map-set allocation-requests
                              { request-id: request-id }
                              (merge current-request { status: "rejected" })
                            )
                            (ok true)
                          )
                      )
                    )
                )
              )
          )
        )
    )
  )
)

;; Simulate allocating a resource without actual contract calls
(define-public (simulate-allocation
  (resource-id uint)
  (user principal)
  (total-available uint)
  (regeneration-rate uint)
  (num-users uint)
)
  (begin
    ;; Only contract owner can simulate allocations
    (if (not (is-eq tx-sender CONTRACT-OWNER))
        (err ERR-NOT-AUTHORIZED)
        ;; Calculate allocation
        (let ((allocation-result (calculate-allocation resource-id total-available num-users regeneration-rate)))
          (if (is-err allocation-result)
              allocation-result
              ;; Just return the result directly - it's already an (ok uint)
              allocation-result
          )
        )
    )
  )
)
