;; metric-verification
;; A contract that implements a verification system for node performance metrics
;; This contract ensures data integrity through multi-party verification, anomaly detection,
;; and reputation tracking for all participants in the verification process.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-UNKNOWN-NODE (err u101))
(define-constant ERR-METRIC-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-VERIFIED (err u103))
(define-constant ERR-VERIFICATION-THRESHOLD-NOT-MET (err u104))
(define-constant ERR-VERIFIER-NOT-REGISTERED (err u105))
(define-constant ERR-METRIC-EXPIRED (err u106))
(define-constant ERR-ANOMALY-DETECTED (err u107))
(define-constant ERR-NOT-OWNER (err u108))
(define-constant ERR-ALREADY-REGISTERED (err u109))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant VERIFICATION-THRESHOLD u3) ;; Number of verifications needed to confirm a metric
(define-constant METRIC-EXPIRY-BLOCKS u144) ;; ~24 hours assuming 10 minute blocks
(define-constant MAX-REPUTATION-SCORE u100)
(define-constant MIN-REPUTATION-SCORE u0)
(define-constant REPUTATION-PENALTY u5)
(define-constant REPUTATION-REWARD u2)
(define-constant ANOMALY-THRESHOLD-PERCENT u20) ;; 20% deviation threshold for anomaly detection

;; Data storage

;; Stores registered nodes
(define-map registered-nodes
  { node-id: (string-ascii 36) }
  { 
    owner: principal,
    active: bool,
    registration-height: uint
  }
)

;; Stores registered verifiers
(define-map verifiers
  { verifier: principal }
  {
    reputation: uint,
    registration-height: uint,
    active: bool
  }
)

;; Stores metrics submitted for verification
(define-map metrics
  { 
    node-id: (string-ascii 36),
    metric-id: (string-ascii 36) 
  }
  {
    reporter: principal,
    value: uint,
    timestamp: uint,
    block-height: uint,
    verified: bool,
    verification-count: uint
  }
)

;; Tracks verifications by verifier for each metric
(define-map verifications
  { 
    node-id: (string-ascii 36),
    metric-id: (string-ascii 36),
    verifier: principal 
  }
  {
    value: uint,
    timestamp: uint,
    block-height: uint
  }
)

;; Stores historical verified metrics for analytics
(define-map verified-metrics
  {
    node-id: (string-ascii 36),
    metric-id: (string-ascii 36),
    block-height: uint
  }
  {
    value: uint,
    verification-count: uint,
    timestamp: uint
  }
)

;; Private functions

;; Validates if a metric has enough verifications to be considered verified
(define-private (check-verification-threshold (node-id (string-ascii 36)) (metric-id (string-ascii 36)))
  (let ((metric (unwrap! (map-get? metrics {node-id: node-id, metric-id: metric-id}) ERR-METRIC-NOT-FOUND)))
    (if (>= (get verification-count metric) VERIFICATION-THRESHOLD)
      (begin
        (map-set metrics 
          {node-id: node-id, metric-id: metric-id}
          (merge metric {verified: true})
        )
        (map-set verified-metrics
          {node-id: node-id, metric-id: metric-id, block-height: (get block-height metric)}
          {
            value: (get value metric),
            verification-count: (get verification-count metric),
            timestamp: (get timestamp metric)
          }
        )
        (ok true)
      )
      ERR-VERIFICATION-THRESHOLD-NOT-MET
    )
  )
)

;; Checks if a metric value is anomalous compared to the submitted value
;; Returns true if the deviation exceeds the threshold
(define-private (is-anomalous (submitted-value uint) (verifier-value uint))
  (let (
    (max-value (if (> submitted-value verifier-value) submitted-value verifier-value))
    (min-value (if (< submitted-value verifier-value) submitted-value verifier-value))
    (deviation-threshold (/ (* submitted-value ANOMALY-THRESHOLD-PERCENT) u100))
  )
    (> (- max-value min-value) deviation-threshold)
  )
)

;; Adjusts verifier reputation based on verification outcomes
(define-private (adjust-reputation (verifier principal) (is-anomaly bool))
  (let ((verifier-data (unwrap! (map-get? verifiers {verifier: verifier}) ERR-VERIFIER-NOT-REGISTERED)))
    (if is-anomaly
      ;; Penalize the verifier for submitting an anomalous value
      (let ((new-reputation (max-int MIN-REPUTATION-SCORE (- (get reputation verifier-data) REPUTATION-PENALTY))))
        (map-set verifiers
          {verifier: verifier}
          (merge verifier-data {reputation: new-reputation})
        )
      )
      ;; Reward the verifier for a valid verification
      (let ((new-reputation (min-int MAX-REPUTATION-SCORE (+ (get reputation verifier-data) REPUTATION-REWARD))))
        (map-set verifiers
          {verifier: verifier}
          (merge verifier-data {reputation: new-reputation})
        )
      )
    )
    (ok true)
  )
)

;; Read-only functions

;; Check if a caller is the contract owner
(define-read-only (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

;; Get node details
(define-read-only (get-node (node-id (string-ascii 36)))
  (map-get? registered-nodes {node-id: node-id})
)

;; Check if node exists and is active
(define-read-only (is-active-node (node-id (string-ascii 36)))
  (match (map-get? registered-nodes {node-id: node-id})
    node-data (get active node-data)
    false
  )
)

;; Get verifier details including reputation
(define-read-only (get-verifier (verifier principal))
  (map-get? verifiers {verifier: verifier})
)

;; Check if principal is a registered verifier
(define-read-only (is-verifier (verifier principal))
  (is-some (map-get? verifiers {verifier: verifier}))
)

;; Get metric details
(define-read-only (get-metric (node-id (string-ascii 36)) (metric-id (string-ascii 36)))
  (map-get? metrics {node-id: node-id, metric-id: metric-id})
)

;; Check if a metric is verified
(define-read-only (is-metric-verified (node-id (string-ascii 36)) (metric-id (string-ascii 36)))
  (match (map-get? metrics {node-id: node-id, metric-id: metric-id})
    metric (get verified metric)
    false
  )
)

;; Get verification count for a specific metric
(define-read-only (get-verification-count (node-id (string-ascii 36)) (metric-id (string-ascii 36)))
  (match (map-get? metrics {node-id: node-id, metric-id: metric-id})
    metric (get verification-count metric)
    u0
  )
)

;; Get a specific verification for a metric by a verifier
(define-read-only (get-verification (node-id (string-ascii 36)) (metric-id (string-ascii 36)) (verifier principal))
  (map-get? verifications {node-id: node-id, metric-id: metric-id, verifier: verifier})
)

;; Get verified metric at a specific block height
(define-read-only (get-verified-metric (node-id (string-ascii 36)) (metric-id (string-ascii 36)) (block-height uint))
  (map-get? verified-metrics {node-id: node-id, metric-id: metric-id, block-height: block-height})
)

;; Public functions

;; Register a new node
(define-public (register-node (node-id (string-ascii 36)))
  (begin
    ;; Check if node already exists
    (asserts! (is-none (map-get? registered-nodes {node-id: node-id})) ERR-ALREADY-REGISTERED)
    
    ;; Register the node
    (map-set registered-nodes
      {node-id: node-id}
      {
        owner: tx-sender,
        active: true,
        registration-height: block-height
      }
    )
    (ok true)
  )
)

;; Deactivate a node
(define-public (deactivate-node (node-id (string-ascii 36)))
  (let ((node-data (unwrap! (map-get? registered-nodes {node-id: node-id}) ERR-UNKNOWN-NODE)))
    ;; Only owner can deactivate
    (asserts! (is-eq tx-sender (get owner node-data)) ERR-NOT-OWNER)
    
    ;; Update node status
    (map-set registered-nodes
      {node-id: node-id}
      (merge node-data {active: false})
    )
    (ok true)
  )
)

;; Register as a verifier
(define-public (register-verifier)
  (begin
    ;; Check if already registered
    (asserts! (is-none (map-get? verifiers {verifier: tx-sender})) ERR-ALREADY-REGISTERED)
    
    ;; Register verifier with initial reputation
    (map-set verifiers
      {verifier: tx-sender}
      {
        reputation: u50, ;; Start with middle-ground reputation
        registration-height: block-height,
        active: true
      }
    )
    (ok true)
  )
)

;; Submit a metric for verification
(define-public (submit-metric (node-id (string-ascii 36)) (metric-id (string-ascii 36)) (value uint) (timestamp uint))
  (let ((node-data (unwrap! (map-get? registered-nodes {node-id: node-id}) ERR-UNKNOWN-NODE)))
    ;; Only node owner can submit metrics
    (asserts! (is-eq tx-sender (get owner node-data)) ERR-NOT-OWNER)
    ;; Check if node is active
    (asserts! (get active node-data) ERR-UNKNOWN-NODE)
    
    ;; Submit the metric
    (map-set metrics
      {node-id: node-id, metric-id: metric-id}
      {
        reporter: tx-sender,
        value: value,
        timestamp: timestamp,
        block-height: block-height,
        verified: false,
        verification-count: u0
      }
    )
    (ok true)
  )
)

;; Verify a metric
(define-public (verify-metric (node-id (string-ascii 36)) (metric-id (string-ascii 36)) (value uint) (timestamp uint))
  (let (
    (metric (unwrap! (map-get? metrics {node-id: node-id, metric-id: metric-id}) ERR-METRIC-NOT-FOUND))
    (verifier-data (unwrap! (map-get? verifiers {verifier: tx-sender}) ERR-VERIFIER-NOT-REGISTERED))
  )
    ;; Check if verifier is active
    (asserts! (get active verifier-data) ERR-VERIFIER-NOT-REGISTERED)
    ;; Check if metric is already verified
    (asserts! (not (get verified metric)) ERR-ALREADY-VERIFIED)
    ;; Check that the metric hasn't expired
    (asserts! (< (- block-height (get block-height metric)) METRIC-EXPIRY-BLOCKS) ERR-METRIC-EXPIRED)
    ;; Check that this verifier hasn't verified this metric already
    (asserts! (is-none (map-get? verifications {node-id: node-id, metric-id: metric-id, verifier: tx-sender})) ERR-ALREADY-VERIFIED)
    
    ;; Check for anomalies
    (let ((anomalous (is-anomalous (get value metric) value)))
      ;; If the value is anomalous, log the verification but adjust reputation negatively
      ;; We still record the verification for analytics purposes
      (map-set verifications
        {node-id: node-id, metric-id: metric-id, verifier: tx-sender}
        {
          value: value,
          timestamp: timestamp,
          block-height: block-height
        }
      )
      
      ;; Adjust verifier reputation based on anomaly status
      (try! (adjust-reputation tx-sender anomalous))
      
      ;; If not anomalous, increase verification count
      (if (not anomalous)
        (begin
          (map-set metrics
            {node-id: node-id, metric-id: metric-id}
            (merge metric {verification-count: (+ (get verification-count metric) u1)})
          )
          
          ;; Check if threshold is met after this verification
          (try! (check-verification-threshold node-id metric-id))
          (ok true)
        )
        ERR-ANOMALY-DETECTED
      )
    )
  )
)

;; Update verifier status (only contract owner can do this)
(define-public (update-verifier-status (verifier principal) (active bool))
  (begin
    ;; Only contract owner can update verifier status
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    (match (map-get? verifiers {verifier: verifier})
      verifier-data (begin
        (map-set verifiers
          {verifier: verifier}
          (merge verifier-data {active: active})
        )
        (ok true)
      )
      ERR-VERIFIER-NOT-REGISTERED
    )
  )
)

;; Update verification threshold (only contract owner can do this)
(define-data-var verification-threshold uint VERIFICATION-THRESHOLD)

(define-public (update-verification-threshold (new-threshold uint))
  (begin
    ;; Only contract owner can update threshold
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set verification-threshold new-threshold)
    (ok true)
  )
)