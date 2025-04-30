;; EcoLedger: Resource Registry Contract (Fixed)
;; Manages registration and allocation of natural resources with regeneration tracking

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
    regeneration-rate: uint,  ;; Amount that regenerates per period
    last-regeneration: uint   ;; Block height of last regeneration
  }
)

;; Resource ownership
(define-map resource-rights
  { resource-id: uint, owner: principal }
  {
    allocation-amount: uint,
    usage-reported: uint      ;; Amount of usage reported by user/sensors
  }
)

;; Resource allocation tracker
(define-map resource-allocations
  { resource-id: uint }
  {
    allocated-amount: uint,   ;; Total amount allocated to all users
    available-amount: uint,   ;; Amount available for allocation
    sustainability-score: uint ;; Score from 0-100 representing sustainability
  }
)

;; Constants for regeneration
(define-constant REGENERATION-PERIOD u144)
(define-constant SUSTAINABILITY-THRESHOLD u70) ;; Threshold for good sustainability score (70%)

;; Resource ID counter
(define-data-var last-resource-id uint u0)

;; Helper Functions

;; Get minimum of two values
(define-read-only (get-min (a uint) (b uint))
  (if (< a b) a b)
)

;; Register a new resource
(define-public (register-resource 
  (name (string-ascii 50)) 
  (resource-type (string-ascii 20)) 
  (total-capacity uint)
  (regeneration-rate uint)
)
  (let ((resource-id (+ u1 (var-get last-resource-id))))
    ;; Only contract owner can register resources
    (if (not (is-eq tx-sender CONTRACT-OWNER))
        (err ERR-NOT-AUTHORIZED)
        (begin
          ;; Register the resource
          (map-set resources
            { resource-id: resource-id }
            {
              name: name,
              resource-type: resource-type,
              total-capacity: total-capacity,
              regeneration-rate: regeneration-rate,
              last-regeneration: block-height
            }
          )
          
          ;; Initialize allocation tracking
          (map-set resource-allocations
            { resource-id: resource-id }
            {
              allocated-amount: u0,
              available-amount: total-capacity,
              sustainability-score: u100 ;; Start at perfect sustainability
            }
          )
          
          ;; Update resource ID counter
          (var-set last-resource-id resource-id)
          
          (ok resource-id)
        )
    )
  )
)

;; Get resource details
(define-read-only (get-resource (resource-id uint))
  (let ((resource (map-get? resources { resource-id: resource-id })))
    (if (is-none resource)
        (err ERR-RESOURCE-NOT-FOUND)
        (ok (unwrap-panic resource))
    )
  )
)

;; Get resource allocation details
(define-read-only (get-resource-allocation (resource-id uint))
  (let ((allocation (map-get? resource-allocations { resource-id: resource-id })))
    (if (is-none allocation)
        (err ERR-RESOURCE-NOT-FOUND)
        (ok (unwrap-panic allocation))
    )
  )
)

;; Check if a principal has rights to a resource
(define-read-only (has-resource-rights (resource-id uint) (owner principal))
  (let ((rights (map-get? resource-rights { resource-id: resource-id, owner: owner })))
    (ok (not (is-none rights)))
  )
)

;; Get the rights details for a specific owner
(define-read-only (get-resource-rights (resource-id uint) (owner principal))
  (let ((rights (map-get? resource-rights { resource-id: resource-id, owner: owner })))
    (if (is-none rights)
        (err ERR-RESOURCE-NOT-FOUND)
        (ok (unwrap-panic rights))
    )
  )
)

;; Calculate time since last regeneration
(define-read-only (get-regeneration-cycles (resource-id uint))
  (let ((resource (map-get? resources { resource-id: resource-id })))
    (if (is-none resource)
        (err ERR-RESOURCE-NOT-FOUND)
        (let ((current-resource (unwrap-panic resource)))
          (ok (/ (- block-height (get last-regeneration current-resource)) REGENERATION-PERIOD))
        )
    )
  )
)

;; Calculate sustainability score based on usage vs. regeneration
(define-read-only (calculate-sustainability (resource-id uint))
  (let ((allocation (map-get? resource-allocations { resource-id: resource-id }))
        (resource (map-get? resources { resource-id: resource-id })))
    (if (or (is-none allocation) (is-none resource))
        (err ERR-RESOURCE-NOT-FOUND)
        (let ((current-allocation (unwrap-panic allocation))
              (current-resource (unwrap-panic resource))
              (allocated (get allocated-amount (unwrap-panic allocation)))
              (total (get total-capacity (unwrap-panic resource)))
              (regen-rate (get regeneration-rate (unwrap-panic resource))))
          ;; Simple sustainability formula: 
          ;; 100 - (allocated / total) * 100 * (regen-rate / total)
          (if (is-eq total u0)
              (ok u0)
              (let ((usage-percent (/ (* allocated u100) total))
                    (regen-factor (if (is-eq total u0) u1 (/ regen-rate total))))
                (ok (if (>= usage-percent u100)
                       u0
                       (- u100 (/ (* usage-percent u100) (+ u100 (* regen-factor u100))))))
              )
          )
        )
    )
  )
)

;; Get a safe sustainability score, defaulting to 0 if error
(define-read-only (get-safe-sustainability-score (resource-id uint))
  (let ((result (calculate-sustainability resource-id)))
    (if (is-ok result)
        (unwrap-panic result)
        u0)
  )
)

;; Allocate resource rights to a principal
(define-public (allocate-resource (resource-id uint) (to principal) (amount uint))
  (if (not (is-eq tx-sender CONTRACT-OWNER))
      (err ERR-NOT-AUTHORIZED)
      (if (<= amount u0)
          (err ERR-INVALID-AMOUNT)
          (let ((resource (map-get? resources { resource-id: resource-id })))
            (if (is-none resource)
                (err ERR-RESOURCE-NOT-FOUND)
                (let ((allocation (map-get? resource-allocations { resource-id: resource-id })))
                  (if (is-none allocation)
                      (err ERR-RESOURCE-NOT-FOUND)
                      (let ((current-allocation (unwrap-panic allocation))
                            (current-available (get available-amount (unwrap-panic allocation))))
                        (if (< current-available amount)
                            (err ERR-ALLOCATION-EXCEEDED)
                            (begin
                              ;; Update resource rights for recipient
                              (let ((existing-rights (map-get? resource-rights { resource-id: resource-id, owner: to })))
                                (if (is-none existing-rights)
                                    ;; Create new rights
                                    (map-set resource-rights
                                      { resource-id: resource-id, owner: to }
                                      { 
                                        allocation-amount: amount,
                                        usage-reported: u0
                                      }
                                    )
                                    ;; Update existing rights
                                    (let ((current-rights (unwrap-panic existing-rights)))
                                      (map-set resource-rights
                                        { resource-id: resource-id, owner: to }
                                        { 
                                          allocation-amount: (+ (get allocation-amount current-rights) amount),
                                          usage-reported: (get usage-reported current-rights)
                                        }
                                      )
                                    )
                                )
                              )
                              
                              ;; Update allocation tracking
                              (map-set resource-allocations
                                { resource-id: resource-id }
                                {
                                  allocated-amount: (+ (get allocated-amount current-allocation) amount),
                                  available-amount: (- current-available amount),
                                  sustainability-score: (get sustainability-score current-allocation)
                                }
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

;; Transfer resource rights between principals
(define-public (transfer-resource-rights (resource-id uint) (to principal) (amount uint))
  (if (<= amount u0)
      (err ERR-INVALID-AMOUNT)
      (let ((sender-rights (map-get? resource-rights { resource-id: resource-id, owner: tx-sender })))
        (if (is-none sender-rights)
            (err ERR-RESOURCE-NOT-FOUND)
            (let ((current-sender-rights (unwrap-panic sender-rights))
                  (sender-amount (get allocation-amount (unwrap-panic sender-rights))))
              (if (< sender-amount amount)
                  (err ERR-INVALID-AMOUNT)
                  (begin
                    ;; Update sender's rights
                    (map-set resource-rights
                      { resource-id: resource-id, owner: tx-sender }
                      { 
                        allocation-amount: (- sender-amount amount),
                        usage-reported: (get usage-reported current-sender-rights)
                      }
                    )
                    
                    ;; Update or create recipient's rights
                    (let ((recipient-rights (map-get? resource-rights { resource-id: resource-id, owner: to })))
                      (if (is-none recipient-rights)
                          ;; Create new rights
                          (map-set resource-rights
                            { resource-id: resource-id, owner: to }
                            { 
                              allocation-amount: amount,
                              usage-reported: u0
                            }
                          )
                          ;; Update existing rights
                          (let ((current-recipient-rights (unwrap-panic recipient-rights)))
                            (map-set resource-rights
                              { resource-id: resource-id, owner: to }
                              { 
                                allocation-amount: (+ (get allocation-amount current-recipient-rights) amount),
                                usage-reported: (get usage-reported current-recipient-rights)
                              }
                            )
                          )
                      )
                    )
                    
                    (ok true)
                  )
              )
            )
        )
      )
  )
)

;; Release allocated resources back to the pool
(define-public (release-resource (resource-id uint) (amount uint))
  (if (<= amount u0)
      (err ERR-INVALID-AMOUNT)
      (let ((rights (map-get? resource-rights { resource-id: resource-id, owner: tx-sender })))
        (if (is-none rights)
            (err ERR-RESOURCE-NOT-FOUND)
            (let ((current-rights (unwrap-panic rights))
                  (current-amount (get allocation-amount (unwrap-panic rights)))
                  (allocation (map-get? resource-allocations { resource-id: resource-id })))
              (if (is-none allocation)
                  (err ERR-RESOURCE-NOT-FOUND)
                  (if (< current-amount amount)
                      (err ERR-INVALID-AMOUNT)
                      (begin
                        ;; Update sender's rights
                        (map-set resource-rights
                          { resource-id: resource-id, owner: tx-sender }
                          { 
                            allocation-amount: (- current-amount amount),
                            usage-reported: (get usage-reported current-rights)
                          }
                        )
                        
                        ;; Update allocation tracking
                        (let ((current-allocation (unwrap-panic allocation)))
                          (map-set resource-allocations
                            { resource-id: resource-id }
                            {
                              allocated-amount: (- (get allocated-amount current-allocation) amount),
                              available-amount: (+ (get available-amount current-allocation) amount),
                              sustainability-score: (get sustainability-score current-allocation)
                            }
                          )
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

;; Report resource usage (can be called by user or authorized sensor)
(define-public (report-usage (resource-id uint) (usage-amount uint))
  (let ((rights (map-get? resource-rights { resource-id: resource-id, owner: tx-sender })))
    (if (is-none rights)
        (err ERR-RESOURCE-NOT-FOUND)
        (let ((current-rights (unwrap-panic rights)))
          (map-set resource-rights
            { resource-id: resource-id, owner: tx-sender }
            { 
              allocation-amount: (get allocation-amount current-rights),
              usage-reported: (+ (get usage-reported current-rights) usage-amount)
            }
          )
          (ok true)
        )
    )
  )
)

;; Process natural regeneration of a resource
(define-public (process-regeneration (resource-id uint))
  (let ((resource (map-get? resources { resource-id: resource-id })))
    (if (is-none resource)
        (err ERR-RESOURCE-NOT-FOUND)
        (let ((current-resource (unwrap-panic resource))
              (allocation (map-get? resource-allocations { resource-id: resource-id })))
          (if (is-none allocation)
              (err ERR-RESOURCE-NOT-FOUND)
              (let ((current-allocation (unwrap-panic allocation))
                    (last-regen (get last-regeneration current-resource))
                    (regen-rate (get regeneration-rate current-resource)))
                ;; Check if enough time has passed for regeneration
                (if (< (- block-height last-regen) REGENERATION-PERIOD)
                    (err ERR-TOO-SOON)
                    (let ((cycles (/ (- block-height last-regen) REGENERATION-PERIOD))
                          (regen-amount (* regen-rate cycles))
                          (total-capacity (get total-capacity current-resource))
                          (current-available (get available-amount current-allocation)))
                      ;; Calculate new available amount, capped at total capacity
                      (let ((new-available (get-min total-capacity (+ current-available regen-amount))))
                        ;; Update resource timestamp
                        (map-set resources
                          { resource-id: resource-id }
                          {
                            name: (get name current-resource),
                            resource-type: (get resource-type current-resource),
                            total-capacity: total-capacity,
                            regeneration-rate: regen-rate,
                            last-regeneration: block-height
                          }
                        )
                        
                        ;; Update allocation with regenerated amount
                        (map-set resource-allocations
                          { resource-id: resource-id }
                          {
                            allocated-amount: (get allocated-amount current-allocation),
                            available-amount: new-available,
                            sustainability-score: (get-safe-sustainability-score resource-id)
                          }
                        )
                        
                        (ok regen-amount)
                      )
                    )
                )
              )
          )
        )
    )
  )
)

;; Add resource capacity (e.g., extraordinary regeneration event)
(define-public (add-resource-capacity (resource-id uint) (amount uint))
  (if (not (is-eq tx-sender CONTRACT-OWNER))
      (err ERR-NOT-AUTHORIZED)
      (if (<= amount u0)
          (err ERR-INVALID-AMOUNT)
          (let ((resource (map-get? resources { resource-id: resource-id })))
            (if (is-none resource)
                (err ERR-RESOURCE-NOT-FOUND)
                (let ((current-resource (unwrap-panic resource))
                      (allocation (map-get? resource-allocations { resource-id: resource-id })))
                  (if (is-none allocation)
                      (err ERR-RESOURCE-NOT-FOUND)
                      (let ((current-allocation (unwrap-panic allocation)))
                        ;; Update resource details
                        (map-set resources
                          { resource-id: resource-id }
                          {
                            name: (get name current-resource),
                            resource-type: (get resource-type current-resource),
                            total-capacity: (+ (get total-capacity current-resource) amount),
                            regeneration-rate: (get regeneration-rate current-resource),
                            last-regeneration: (get last-regeneration current-resource)
                          }
                        )
                        
                        ;; Update allocation tracking
                        (map-set resource-allocations
                          { resource-id: resource-id }
                          {
                            allocated-amount: (get allocated-amount current-allocation),
                            available-amount: (+ (get available-amount current-allocation) amount),
                            sustainability-score: (get sustainability-score current-allocation)
                          }
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

;; Get resource usage statistics
(define-read-only (get-resource-usage-stats (resource-id uint))
  (let ((resource (map-get? resources { resource-id: resource-id }))
        (allocation (map-get? resource-allocations { resource-id: resource-id })))
    (if (or (is-none resource) (is-none allocation))
        (err ERR-RESOURCE-NOT-FOUND)
        (let ((current-resource (unwrap-panic resource))
              (current-allocation (unwrap-panic allocation)))
          (ok {
            total-capacity: (get total-capacity current-resource),
            allocated-amount: (get allocated-amount current-allocation),
            available-amount: (get available-amount current-allocation),
            regeneration-rate: (get regeneration-rate current-resource),
            sustainability-score: (get sustainability-score current-allocation),
            next-regeneration-height: (+ (get last-regeneration current-resource) REGENERATION-PERIOD)
          })
        )
    )
  )
)
