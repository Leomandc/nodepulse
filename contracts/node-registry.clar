;; node-registry
;; This contract serves as the central registry for all nodes being monitored in the NodePulse system.
;; It manages registration and identity of blockchain nodes, allowing operators to register nodes
;; with essential information and maintaining a directory of available nodes with their statuses.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NODE-ALREADY-REGISTERED (err u101))
(define-constant ERR-NODE-NOT-FOUND (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-INVALID-NETWORK-TYPE (err u104))
(define-constant ERR-INVALID-PARAMS (err u105))

;; Status codes for nodes
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-INACTIVE u2)
(define-constant STATUS-MAINTENANCE u3)

;; Network types
(define-constant NETWORK-BITCOIN u1)
(define-constant NETWORK-STACKS u2)
(define-constant NETWORK-ETHEREUM u3)
(define-constant NETWORK-OTHER u4)

;; Data maps and variables
;; Counter for unique node IDs
(define-data-var next-node-id uint u1)

;; Map of node IDs to node information
(define-map nodes
  { node-id: uint }
  {
    owner: principal,
    name: (string-ascii 100),
    network-type: uint,
    location: (string-ascii 100),
    hardware-specs: (string-ascii 255),
    status: uint,
    verified: bool,
    registration-time: uint
  }
)

;; Map to track nodes by owner for easy lookup
(define-map nodes-by-owner
  { owner: principal }
  { node-ids: (list 100 uint) }
)

;; Private functions

;; Helper function to check if a status code is valid
(define-private (is-valid-status (status uint))
  (or
    (is-eq status STATUS-ACTIVE)
    (is-eq status STATUS-INACTIVE)
    (is-eq status STATUS-MAINTENANCE)
  )
)

;; Helper function to check if a network type is valid
(define-private (is-valid-network-type (network-type uint))
  (or
    (is-eq network-type NETWORK-BITCOIN)
    (is-eq network-type NETWORK-STACKS)
    (is-eq network-type NETWORK-ETHEREUM)
    (is-eq network-type NETWORK-OTHER)
  )
)

;; Helper function to add a node ID to owner's list
(define-private (add-node-to-owner (owner principal) (node-id uint))
  (let ((current-nodes (default-to { node-ids: (list) } (map-get? nodes-by-owner { owner: owner }))))
    (map-set nodes-by-owner
      { owner: owner }
      { node-ids: (unwrap-panic (as-max-len? (append (get node-ids current-nodes) node-id) u100)) }
    )
  )
)

;; Read-only functions

;; Get node details by node ID
(define-read-only (get-node (node-id uint))
  (map-get? nodes { node-id: node-id })
)

;; Get all nodes owned by a specific principal
(define-read-only (get-nodes-by-owner (owner principal))
  (default-to { node-ids: (list) } (map-get? nodes-by-owner { owner: owner }))
)

;; Check if a node exists
(define-read-only (node-exists? (node-id uint))
  (is-some (map-get? nodes { node-id: node-id }))
)

;; Get the current count of registered nodes
(define-read-only (get-node-count)
  (- (var-get next-node-id) u1)
)

;; Public functions

;; Register a new node
(define-public (register-node
  (name (string-ascii 100))
  (network-type uint)
  (location (string-ascii 100))
  (hardware-specs (string-ascii 255))
  (status uint))
  
  (let
    ((node-id (var-get next-node-id))
     (current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    
    ;; Input validation
    (asserts! (is-valid-network-type network-type) ERR-INVALID-NETWORK-TYPE)
    (asserts! (is-valid-status status) ERR-INVALID-STATUS)
    (asserts! (> (len name) u0) ERR-INVALID-PARAMS)
    
    ;; Store node data
    (map-set nodes
      { node-id: node-id }
      {
        owner: tx-sender,
        name: name,
        network-type: network-type,
        location: location,
        hardware-specs: hardware-specs,
        status: status,
        verified: false,
        registration-time: current-time
      }
    )
    
    ;; Update owner's node list
    (add-node-to-owner tx-sender node-id)
    
    ;; Increment node ID counter for next registration
    (var-set next-node-id (+ node-id u1))
    
    ;; Return success with the assigned node ID
    (ok node-id)
  )
)

;; Update node status
(define-public (update-node-status (node-id uint) (new-status uint))
  (let ((node (map-get? nodes { node-id: node-id })))
    ;; Check if node exists and sender is the owner
    (asserts! (is-some node) ERR-NODE-NOT-FOUND)
    (asserts! (is-eq (get owner (unwrap-panic node)) tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Validate status code
    (asserts! (is-valid-status new-status) ERR-INVALID-STATUS)
    
    ;; Update node status
    (map-set nodes
      { node-id: node-id }
      (merge (unwrap-panic node) { status: new-status })
    )
    
    (ok true)
  )
)

;; Update node information
(define-public (update-node-info
  (node-id uint)
  (name (string-ascii 100))
  (network-type uint)
  (location (string-ascii 100))
  (hardware-specs (string-ascii 255)))
  
  (let ((node (map-get? nodes { node-id: node-id })))
    ;; Check if node exists and sender is the owner
    (asserts! (is-some node) ERR-NODE-NOT-FOUND)
    (asserts! (is-eq (get owner (unwrap-panic node)) tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Validate inputs
    (asserts! (is-valid-network-type network-type) ERR-INVALID-NETWORK-TYPE)
    (asserts! (> (len name) u0) ERR-INVALID-PARAMS)
    
    ;; Update node information
    (map-set nodes
      { node-id: node-id }
      (merge (unwrap-panic node)
        {
          name: name,
          network-type: network-type,
          location: location,
          hardware-specs: hardware-specs
        }
      )
    )
    
    (ok true)
  )
)

;; Admin function to verify a node (could be connected to a DAO or governance contract)
;; In a production environment, this would have additional authorization controls
(define-public (verify-node (node-id uint) (verification-status bool))
  ;; For now, only contract owner can verify nodes
  ;; In production, this would be replaced with proper governance mechanism
  (let ((node (map-get? nodes { node-id: node-id })))
    ;; Check if node exists
    (asserts! (is-some node) ERR-NODE-NOT-FOUND)
    ;; Simple authorization check - would be enhanced in production
    (asserts! (is-eq tx-sender (as-contract tx-sender)) ERR-NOT-AUTHORIZED)
    
    ;; Update verification status
    (map-set nodes
      { node-id: node-id }
      (merge (unwrap-panic node) { verified: verification-status })
    )
    
    (ok true)
  )
)

;; Deregister a node - allows node owners to remove their nodes from the registry
(define-public (deregister-node (node-id uint))
  (let ((node (map-get? nodes { node-id: node-id })))
    ;; Check if node exists and sender is the owner
    (asserts! (is-some node) ERR-NODE-NOT-FOUND)
    (asserts! (is-eq (get owner (unwrap-panic node)) tx-sender) ERR-NOT-AUTHORIZED)
    
    ;; Delete node from registry
    (map-delete nodes { node-id: node-id })
    
    ;; Node IDs in owner's list would remain, but the node itself would be gone
    ;; A production version might want to also remove the ID from the owner's list
    
    (ok true)
  )
)