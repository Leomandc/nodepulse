;; NodePulse Governance Contract
;; This contract enables decentralized governance for the NodePulse system through voting,
;; allowing stakeholders to propose and decide on parameter changes, upgrades, and dispute resolutions.
;; By decentralizing control, NodePulse remains neutral, adaptive, and resistant to manipulation.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-PROPOSAL-EXPIRED (err u102))
(define-constant ERR-PROPOSAL-ACTIVE (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-INSUFFICIENT-STAKE (err u105))
(define-constant ERR-INVALID-PROPOSAL-TYPE (err u106))
(define-constant ERR-INVALID-PARAMETER (err u107))
(define-constant ERR-DISPUTE-NOT-FOUND (err u108))
(define-constant ERR-DISPUTE-RESOLVED (err u109))
(define-constant ERR-VOTING-PERIOD-ENDED (err u110))

;; Constants
(define-constant PROPOSAL-DURATION u10080) ;; Duration in blocks (approx. 1 week)
(define-constant MIN-STAKE-REQUIREMENT u1000000) ;; Minimum stake required to create proposal (in uSTX)
(define-constant VOTE-THRESHOLD-PERCENT u60) ;; 60% vote threshold for proposal to pass

;; Proposal types
(define-constant PARAM-CHANGE u1)
(define-constant PROTOCOL-UPGRADE u2)
(define-constant DISPUTE-RESOLUTION u3)

;; Data structures
(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-utf8 1000),
    proposal-type: uint,
    parameter-key: (optional (string-ascii 50)),
    parameter-value: (optional (string-utf8 500)),
    creation-height: uint,
    expiration-height: uint,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 20), ;; "active", "passed", "rejected", "executed"
    executed: bool,
    target-dispute-id: (optional uint) ;; Add field for dispute resolution proposals
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { 
    vote: bool, ;; true = for, false = against
    weight: uint 
  }
)

(define-map disputes
  { dispute-id: uint }
  {
    proposal-id: uint,
    reporter: principal,
    defendant: principal,
    evidence: (string-utf8 1000),
    resolution: (optional (string-utf8 500)),
    resolved: bool
  }
)

(define-map system-parameters
  { param-key: (string-ascii 50) }
  { param-value: (string-utf8 500) }
)

;; Data variables
(define-data-var next-proposal-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var admin principal tx-sender)

;; Private functions
(define-private (is-admin)
  (is-eq tx-sender (var-get admin))
)

(define-private (calculate-voting-power (account principal))
  ;; In a real implementation, this would query a token contract
  ;; or a staking contract to determine voting power
  ;; For simplicity, we'll return a placeholder value here
  u1
)

(define-private (validate-proposal-type (proposal-type uint))
  (or 
    (is-eq proposal-type PARAM-CHANGE)
    (or 
      (is-eq proposal-type PROTOCOL-UPGRADE)
      (is-eq proposal-type DISPUTE-RESOLUTION)
    )
  )
)

(define-private (is-proposal-active (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (and 
              (is-eq (get status proposal) "active")
              (<= block-height (get expiration-height proposal))
            )
    false
  )
)


(define-private (check-and-resolve-dispute (dispute-entry { 
    id: uint, 
    data: {
      proposal-id: uint,
      reporter: principal,
      defendant: principal,
      evidence: (string-utf8 1000),
      resolution: (optional (string-utf8 500)),
      resolved: bool
    }
  }) (acc (response bool uint))) ;; item, accumulator
  (let (
    (dispute-id (get id dispute-entry))
    (dispute-data (get data dispute-entry))
  )
    (if (get resolved dispute-data) ;; If already resolved, return accumulator unchanged
        acc
        ;; Else, try to resolve
        (match (map-get? proposals { proposal-id: (get proposal-id dispute-data) })
          proposal ;; Found associated proposal
            (if (and (is-eq (get proposal-type proposal) DISPUTE-RESOLUTION) ;; Check type
                     (is-some (get target-dispute-id proposal)) ;; Check target ID exists
                     (is-eq (unwrap-panic (get target-dispute-id proposal)) dispute-id)) ;; Check target ID matches
              (begin ;; IDs match, resolve the dispute
                (map-set disputes
                  { dispute-id: dispute-id }
                  (merge dispute-data { resolution: (get parameter-value proposal), resolved: true }))
                (ok true)) ;; Return success as the new accumulator
              acc) ;; Proposal doesn't match, return accumulator unchanged
          acc ;; Proposal not found, return accumulator unchanged
        )
    )
  )
)

;; Public functions
(define-public (create-proposal 
  (title (string-ascii 100)) 
  (description (string-utf8 1000)) 
  (proposal-type uint)
  (parameter-key (optional (string-ascii 50)))
  (parameter-value (optional (string-utf8 500))))
  
  (let (
    (stake-amount (calculate-voting-power tx-sender))
    (proposal-id (var-get next-proposal-id))
  )
    ;; Check if sender has enough stake
    (asserts! (>= stake-amount MIN-STAKE-REQUIREMENT) ERR-INSUFFICIENT-STAKE)
    
    ;; Validate proposal type
    (asserts! (validate-proposal-type proposal-type) ERR-INVALID-PROPOSAL-TYPE)
    
    ;; Create the proposal
    (map-set proposals 
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        proposal-type: proposal-type,
        parameter-key: parameter-key,
        parameter-value: parameter-value,
        creation-height: block-height,
        expiration-height: (+ block-height PROPOSAL-DURATION),
        votes-for: u0,
        votes-against: u0,
        status: "active",
        executed: false,
        target-dispute-id: none
      }
    )
    
    ;; Increment proposal ID
    (var-set next-proposal-id (+ proposal-id u1))
    
    (ok proposal-id)
  )
)


(define-public (create-dispute 
  (defendant principal) 
  (evidence (string-utf8 1000)))
  
  (let (
    (dispute-id (var-get next-dispute-id))
  )
    ;; Record the dispute
    (map-set disputes 
      { dispute-id: dispute-id }
      {
        proposal-id: u0, ;; Will be set when dispute resolution proposal is created
        reporter: tx-sender,
        defendant: defendant,
        evidence: evidence,
        resolution: none,
        resolved: false
      }
    )
    
    ;; Increment dispute ID
    (var-set next-dispute-id (+ dispute-id u1))
    
    (ok dispute-id)
  )
)

(define-public (create-dispute-resolution-proposal 
  (dispute-id uint) 
  (title (string-ascii 100)) 
  (description (string-utf8 1000))
  (resolution (string-utf8 500)))
  
  (match (map-get? disputes { dispute-id: dispute-id })
    dispute (begin
      ;; Check if dispute is not already resolved
      (asserts! (not (get resolved dispute)) ERR-DISPUTE-RESOLVED)
      
      ;; Create a proposal for dispute resolution
      (let (
        (proposal-result (create-proposal 
                           title 
                           description 
                           DISPUTE-RESOLUTION
                           none ;; Pass none for parameter-key
                           (some resolution)))
        (proposal-id (unwrap! proposal-result ERR-INVALID-PARAMETER))
        ;; Get the newly created proposal to update it
        (proposal-data (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
      )
        ;; Update the proposal with the target dispute ID
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal-data { target-dispute-id: (some dispute-id) })
        )
        ;; Update dispute with proposal ID
        (map-set disputes 
          { dispute-id: dispute-id }
          (merge dispute { proposal-id: proposal-id })
        )
        
        (ok proposal-id)
      ))
    ERR-DISPUTE-NOT-FOUND)
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: voter }))
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-proposal-count)
  (- (var-get next-proposal-id) u1)
)

(define-read-only (get-dispute-count)
  (- (var-get next-dispute-id) u1)
)

;; Admin functions
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (var-set admin new-admin)
    (ok true)
  )
)

(define-public (set-system-parameter (param-key (string-ascii 50)) (param-value (string-utf8 500)))
  (begin
    (asserts! (is-admin) ERR-NOT-AUTHORIZED)
    (map-set system-parameters { param-key: param-key } { param-value: param-value })
    (ok true)
  )
)
