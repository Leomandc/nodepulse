;; performance-reporting
;; A contract for collecting and storing node performance metrics on the Stacks blockchain.
;; This contract enables node operators to submit performance data and users to view historical performance metrics.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NODE-NOT-REGISTERED (err u101))
(define-constant ERR-INVALID-METRIC (err u102))
(define-constant ERR-INVALID-VALUE (err u103))
(define-constant ERR-DUPLICATE-REPORT (err u104))
(define-constant ERR-MAX-REPORTS-REACHED (err u105))

;; Data space definitions
;; Map to store node ownership: node-id -> owner address
(define-map node-owners uint principal)

;; Map to store registered nodes: node-id -> true
(define-map registered-nodes uint bool)

;; Map to track the number of reports submitted per node: node-id -> count
(define-map report-counts uint uint)

;; Map to store performance metrics: {node-id, timestamp} -> {uptime, response-time, block-prop-speed, tx-throughput}
(define-map performance-metrics 
  { node-id: uint, timestamp: uint } 
  { 
    uptime: uint,                ;; percentage (0-100)
    response-time: uint,         ;; milliseconds
    block-prop-speed: uint,      ;; milliseconds
    tx-throughput: uint          ;; transactions per second * 100 (to avoid decimals)
  }
)

;; Map to store the timestamps of reports for each node (as a linked list)
;; node-id -> {latest-timestamp, previous-timestamp}
(define-map report-timestamps 
  uint 
  { latest-timestamp: uint, previous-timestamp: uint }
)

;; Constants
(define-constant MAX-REPORTS-PER-NODE u1000)
(define-constant ADMIN-ADDRESS tx-sender) ;; Set contract deployer as admin

;; Private functions
(define-private (is-node-owner (node-id uint))
  ;; Check if tx-sender is the owner of the specified node
  (let ((owner (get-node-owner node-id)))
    (is-eq (ok tx-sender) owner)
  )
)

(define-private (update-report-timestamps (node-id uint) (timestamp uint))
  ;; Updates the linked list of timestamps for a node's reports
  (let (
    (current-data (default-to { latest-timestamp: u0, previous-timestamp: u0 } 
                  (map-get? report-timestamps node-id)))
    (latest-timestamp (get latest-timestamp current-data))
  )
    (map-set report-timestamps node-id 
      { latest-timestamp: timestamp, previous-timestamp: latest-timestamp })
  )
)

(define-private (increment-report-count (node-id uint))
  ;; Increment the count of reports for a node
  (let ((current-count (default-to u0 (map-get? report-counts node-id))))
    (map-set report-counts node-id (+ current-count u1))
    (< current-count MAX-REPORTS-PER-NODE)
  )
)

(define-private (validate-metrics 
  (uptime uint) 
  (response-time uint) 
  (block-prop-speed uint) 
  (tx-throughput uint)
)
  ;; Validate that the metrics are within acceptable ranges
  (and
    (<= uptime u100)  ;; Uptime is a percentage (0-100)
    (> response-time u0)
    (> block-prop-speed u0)
    (>= tx-throughput u0)
  )
)

(define-private (is-report-exists (node-id uint) (timestamp uint))
  ;; Check if a report already exists for the given node and timestamp
  (is-some (map-get? performance-metrics { node-id: node-id, timestamp: timestamp }))
)

;; Read-only functions
(define-read-only (get-node-owner (node-id uint))
  ;; Retrieve the owner of a node
  (let ((owner (map-get? node-owners node-id)))
    (if (is-some owner)
      (ok (unwrap-panic owner))
      (err ERR-NODE-NOT-REGISTERED)
    )
  )
)

(define-read-only (get-performance-metrics (node-id uint) (timestamp uint))
  ;; Retrieve performance metrics for a specific node at a specific timestamp
  (let ((metrics (map-get? performance-metrics { node-id: node-id, timestamp: timestamp })))
    (if (is-some metrics)
      (ok (unwrap-panic metrics))
      (err ERR-INVALID-METRIC)
    )
  )
)

(define-read-only (get-latest-performance (node-id uint))
  ;; Retrieve the most recent performance metrics for a node
  (let (
    (timestamps (map-get? report-timestamps node-id))
  )
    (if (is-some timestamps)
      (let ((latest-ts (get latest-timestamp (unwrap-panic timestamps))))
        (if (> latest-ts u0)
          (get-performance-metrics node-id latest-ts)
          (err ERR-INVALID-METRIC)
        )
      )
      (err ERR-NODE-NOT-REGISTERED)
    )
  )
)

(define-read-only (get-report-count (node-id uint))
  ;; Get the number of reports submitted for a node
  (default-to u0 (map-get? report-counts node-id))
)

(define-read-only (get-report-timestamps (node-id uint))
  ;; Get the timestamps of reports for a node
  (map-get? report-timestamps node-id)
)

(define-read-only (is-node-registered (node-id uint))
  ;; Check if a node is registered
  (default-to false (map-get? registered-nodes node-id))
)

;; Public functions
(define-public (register-node (node-id uint))
  ;; Register a new node with the specified ID
  (begin
    (asserts! (not (is-node-registered node-id)) (err ERR-DUPLICATE-REPORT))
    
    ;; Set the owner and mark as registered
    (map-set node-owners node-id tx-sender)
    (map-set registered-nodes node-id true)
    
    ;; Initialize report count
    (map-set report-counts node-id u0)
    
    (ok true)
  )
)

(define-public (submit-performance-report 
  (node-id uint) 
  (timestamp uint) 
  (uptime uint) 
  (response-time uint) 
  (block-prop-speed uint) 
  (tx-throughput uint)
)
  ;; Submit a new performance report for a node
  (let (
    (can-submit (and 
                  (is-node-registered node-id)
                  (is-node-owner node-id)
                  (validate-metrics uptime response-time block-prop-speed tx-throughput)
                  (not (is-report-exists node-id timestamp))
                  (increment-report-count node-id)
                ))
  )
    (asserts! can-submit (err ERR-NOT-AUTHORIZED))
    
    ;; Store the metrics
    (map-set performance-metrics 
      { node-id: node-id, timestamp: timestamp }
      { 
        uptime: uptime, 
        response-time: response-time, 
        block-prop-speed: block-prop-speed, 
        tx-throughput: tx-throughput 
      }
    )
    
    ;; Update the timestamps linked list
    (update-report-timestamps node-id timestamp)
    
    (ok true)
  )
)

(define-public (transfer-node-ownership (node-id uint) (new-owner principal))
  ;; Transfer ownership of a node to a new address
  (begin
    (asserts! (is-node-owner node-id) (err ERR-NOT-AUTHORIZED))
    
    ;; Update the owner
    (map-set node-owners node-id new-owner)
    
    (ok true)
  )
)

(define-public (admin-deregister-node (node-id uint))
  ;; Admin function to deregister a node (e.g., in case of abuse)
  (begin
    (asserts! (is-eq tx-sender ADMIN-ADDRESS) (err ERR-NOT-AUTHORIZED))
    (asserts! (is-node-registered node-id) (err ERR-NODE-NOT-REGISTERED))
    
    ;; Remove node registration
    (map-delete registered-nodes node-id)
    
    (ok true)
  )
)