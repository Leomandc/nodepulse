;; rewards-distribution
;; A contract that incentivizes accurate reporting and high-performing nodes through token rewards.
;; This contract creates an economic incentive system that distributes rewards to verifiers who
;; consistently provide accurate validations and to node operators who maintain excellent performance metrics.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-NODE (err u101))
(define-constant ERR-INVALID-VERIFIER (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-REWARD-CLAIMED (err u104))
(define-constant ERR-NO-REWARDS-AVAILABLE (err u105))
(define-constant ERR-INVALID-TIMESTAMP (err u106))
(define-constant ERR-DISTRIBUTION-LOCKED (err u107))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant BLOCKS-PER-EPOCH u144) ;; Approximately 1 day of blocks on Stacks
(define-constant PERFORMANCE-WEIGHT u70) ;; 70% weight for performance metrics
(define-constant COMMUNITY-FEEDBACK-WEIGHT u30) ;; 30% weight for community feedback
(define-constant MINIMUM-VERIFICATIONS-PER-EPOCH u5) ;; Minimum verifications required to be eligible for rewards

;; Data maps
;; Track registered nodes
(define-map nodes principal { 
    registered: bool,
    last-reward-epoch: uint,
    uptime-score: uint,
    response-time-score: uint,
    community-rating: uint
})

;; Track registered verifiers
(define-map verifiers principal {
    registered: bool,
    last-reward-epoch: uint, 
    correct-validations: uint,
    total-validations: uint,
    reputation-score: uint
})

;; Track node performance reports
(define-map node-performance-reports 
    { node: principal, epoch: uint } 
    { uptime: uint, response-time: uint, verified: bool }
)

;; Track verifier submissions
(define-map verifier-submissions
    { verifier: principal, node: principal, epoch: uint }
    { submitted: bool, accurate: bool }
)

;; Track reward distribution by epoch
(define-map epoch-rewards
    uint
    { 
        node-rewards-pool: uint,
        verifier-rewards-pool: uint,
        distributed: bool 
    }
)

;; Track claimed rewards
(define-map claimed-rewards
    { participant: principal, epoch: uint }
    { claimed: bool, amount: uint }
)

;; Data vars
(define-data-var current-epoch uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var reward-token-balance uint u0)
(define-data-var distribution-enabled bool true)

;; Private functions

;; Calculate the current epoch based on block height
(define-private (calculate-current-epoch)
    (/ block-height BLOCKS-PER-EPOCH)
)

;; Calculate node reward based on performance metrics
(define-private (calculate-node-reward (node principal) (epoch uint))
    (let (
        (node-data (default-to { registered: false, last-reward-epoch: u0, uptime-score: u0, response-time-score: u0, community-rating: u0 } (map-get? nodes node)))
        (performance-reports (default-to { uptime: u0, response-time: u0, verified: false } (map-get? node-performance-reports { node: node, epoch: epoch })))
        (epoch-reward-data (default-to { node-rewards-pool: u0, verifier-rewards-pool: u0, distributed: false } (map-get? epoch-rewards epoch)))
        (performance-score (+ (* (get uptime-score node-data) PERFORMANCE-WEIGHT) (* (get community-rating node-data) COMMUNITY-FEEDBACK-WEIGHT)))
        (node-share (/ (* performance-score u100) u10000)) ;; Performance score as percentage of maximum
    )
    (if (and (get registered node-data) (get verified performance-reports))
        (/ (* (get node-rewards-pool epoch-reward-data) node-share) u100)
        u0
    ))
)

;; Calculate verifier reward based on accuracy and activity
(define-private (calculate-verifier-reward (verifier principal) (epoch uint))
    (let (
        (verifier-data (default-to { registered: false, last-reward-epoch: u0, correct-validations: u0, total-validations: u0, reputation-score: u0 } (map-get? verifiers verifier)))
        (epoch-reward-data (default-to { node-rewards-pool: u0, verifier-rewards-pool: u0, distributed: false } (map-get? epoch-rewards epoch)))
        (correct-validations (get correct-validations verifier-data))
        (total-validations (get total-validations verifier-data))
        (accuracy-score (if (> total-validations u0) 
                           (/ (* correct-validations u100) total-validations)
                           u0))
        (activity-factor (if (>= total-validations MINIMUM-VERIFICATIONS-PER-EPOCH) u100 (/ (* total-validations u100) MINIMUM-VERIFICATIONS-PER-EPOCH)))
        (verifier-share (/ (* accuracy-score activity-factor) u10000))
    )
    (if (get registered verifier-data)
        (/ (* (get verifier-rewards-pool epoch-reward-data) verifier-share) u100)
        u0
    ))
)

;; Check if an epoch is ready for distribution
(define-private (is-epoch-eligible-for-distribution (epoch uint))
    (let (
        (current-epoch-val (var-get current-epoch))
        (epoch-data (default-to { node-rewards-pool: u0, verifier-rewards-pool: u0, distributed: false } (map-get? epoch-rewards epoch)))
    )
    (and 
        (< epoch current-epoch-val)  ;; Only past epochs can be distributed
        (not (get distributed epoch-data))  ;; Has not been distributed yet
        (var-get distribution-enabled)  ;; Global distribution switch is on
    ))
)

;; Public functions

;; Register a node for reward eligibility
(define-public (register-node)
    (let (
        (sender tx-sender)
    )
    (if (is-some (map-get? nodes sender))
        (ok true)  ;; Already registered
        (begin
            (map-set nodes sender {
                registered: true,
                last-reward-epoch: u0,
                uptime-score: u0,  ;; Initial score, will be updated with performance data
                response-time-score: u0,  ;; Initial score, will be updated with performance data
                community-rating: u0  ;; Initial rating, will be updated with feedback
            })
            (ok true)
        ))
    )
)

;; Register a verifier for reward eligibility
(define-public (register-verifier)
    (let (
        (sender tx-sender)
    )
    (if (is-some (map-get? verifiers sender))
        (ok true)  ;; Already registered
        (begin
            (map-set verifiers sender {
                registered: true,
                last-reward-epoch: u0,
                correct-validations: u0,
                total-validations: u0,
                reputation-score: u0
            })
            (ok true)
        ))
    )
)

;; Submit node performance data (only authorized verifiers can call)
(define-public (submit-node-performance (node principal) (uptime uint) (response-time uint))
    (let (
        (sender tx-sender)
        (current-epoch-val (calculate-current-epoch))
        (verifier-data (default-to { registered: false, last-reward-epoch: u0, correct-validations: u0, total-validations: u0, reputation-score: u0 } (map-get? verifiers sender)))
        (node-data (default-to { registered: false, last-reward-epoch: u0, uptime-score: u0, response-time-score: u0, community-rating: u0 } (map-get? nodes node)))
    )
    (asserts! (get registered verifier-data) ERR-NOT-AUTHORIZED)
    (asserts! (get registered node-data) ERR-INVALID-NODE)
    (asserts! (<= uptime u100) ERR-INVALID-AMOUNT)  ;; Uptime as percentage (0-100)
    
    ;; Record the submission
    (map-set verifier-submissions 
        { verifier: sender, node: node, epoch: current-epoch-val }
        { submitted: true, accurate: false }  ;; Accuracy will be determined later through consensus
    )
    
    ;; Update performance report (multiple verifiers may submit, final verification happens separately)
    (map-set node-performance-reports
        { node: node, epoch: current-epoch-val }
        { uptime: uptime, response-time: response-time, verified: false }
    )
    
    ;; Update verifier's total validations count
    (map-set verifiers sender {
        registered: (get registered verifier-data),
        last-reward-epoch: (get last-reward-epoch verifier-data),
        correct-validations: (get correct-validations verifier-data),
        total-validations: (+ (get total-validations verifier-data) u1),
        reputation-score: (get reputation-score verifier-data)
    })
    
    (ok true)
    )
)

;; Verify node performance data through consensus (only contract owner can call)
;; In a production system, this would likely be handled by a decentralized oracle or multi-sig mechanism
(define-public (verify-node-performance (node principal) (epoch uint) (verified bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= epoch (calculate-current-epoch)) ERR-INVALID-TIMESTAMP)
        
        (let (
            (performance-data (default-to { uptime: u0, response-time: u0, verified: false } 
                              (map-get? node-performance-reports { node: node, epoch: epoch })))
        )
            ;; Update verification status
            (map-set node-performance-reports
                { node: node, epoch: epoch }
                (merge performance-data { verified: verified })
            )
            
            ;; If verified, update node performance metrics
            (if verified
                (let (
                    (node-data (default-to { registered: false, last-reward-epoch: u0, uptime-score: u0, response-time-score: u0, community-rating: u0 } 
                                         (map-get? nodes node)))
                    (new-uptime-score (get uptime performance-data))
                    (new-response-time-score (if (> (get response-time performance-data) u0)
                                              (/ u10000 (get response-time performance-data))  ;; Lower response time is better
                                              u100))
                )
                    (map-set nodes node {
                        registered: (get registered node-data),
                        last-reward-epoch: epoch,
                        uptime-score: new-uptime-score,
                        response-time-score: new-response-time-score,
                        community-rating: (get community-rating node-data)
                    })
                )
             true ;; Add the missing else branch
            )
            
            (ok true)
        )
    )
)

;; Update verifier accuracy based on consensus (only contract owner can call)
(define-public (update-verifier-accuracy (verifier principal) (node principal) (epoch uint) (accurate bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= epoch (calculate-current-epoch)) ERR-INVALID-TIMESTAMP)
        
        (let (
            (submission (default-to { submitted: false, accurate: false } 
                        (map-get? verifier-submissions { verifier: verifier, node: node, epoch: epoch })))
            (verifier-data (default-to { registered: false, last-reward-epoch: u0, correct-validations: u0, total-validations: u0, reputation-score: u0 } 
                                     (map-get? verifiers verifier)))
        )
            (asserts! (get submitted submission) ERR-INVALID-VERIFIER)
            
            ;; Update submission accuracy
            (map-set verifier-submissions
                { verifier: verifier, node: node, epoch: epoch }
                { submitted: true, accurate: accurate }
            )
            
            ;; Update verifier stats if submission was accurate
            (if accurate
                (map-set verifiers verifier {
                    registered: (get registered verifier-data),
                    last-reward-epoch: epoch,
                    correct-validations: (+ (get correct-validations verifier-data) u1),
                    total-validations: (get total-validations verifier-data),
                    reputation-score: (+ (get reputation-score verifier-data) u1)
                })
              true) ;; Use 'true' for the else branch when no action is needed
            
            (ok true)
        )
    )
)

;; Set reward pools for an epoch (only contract owner can call)
(define-public (set-epoch-rewards (epoch uint) (node-pool uint) (verifier-pool uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (map-set epoch-rewards epoch {
            node-rewards-pool: node-pool,
            verifier-rewards-pool: verifier-pool,
            distributed: false
        })
        
        ;; Update token balance
        (var-set reward-token-balance (+ (var-get reward-token-balance) (+ node-pool verifier-pool)))
        
        (ok true)
    )
)

;; Advance to next epoch (only contract owner can call)
(define-public (advance-epoch)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (var-set current-epoch (+ (var-get current-epoch) u1))
        
        (ok (var-get current-epoch))
    )
)

;; Claim rewards for a specific epoch
(define-public (claim-rewards (epoch uint))
    (let (
        (sender tx-sender)
        (is-node (default-to false (get registered (map-get? nodes sender))))
        (is-verifier (default-to false (get registered (map-get? verifiers sender))))
        (already-claimed (default-to false (get claimed (map-get? claimed-rewards { participant: sender, epoch: epoch }))))
        (current-epoch-val (var-get current-epoch))
    )
        (asserts! (or is-node is-verifier) ERR-NOT-AUTHORIZED)
        (asserts! (not already-claimed) ERR-REWARD-CLAIMED)
        (asserts! (< epoch current-epoch-val) ERR-INVALID-TIMESTAMP)
        (asserts! (is-epoch-eligible-for-distribution epoch) ERR-DISTRIBUTION-LOCKED)
        
        (let (
            (node-reward (if is-node (calculate-node-reward sender epoch) u0))
            (verifier-reward (if is-verifier (calculate-verifier-reward sender epoch) u0))
            (total-reward (+ node-reward verifier-reward))
        )
            (asserts! (> total-reward u0) ERR-NO-REWARDS-AVAILABLE)
            
            ;; Record the claim
            (map-set claimed-rewards 
                { participant: sender, epoch: epoch }
                { claimed: true, amount: total-reward }
            )
            
            ;; Update total rewards distributed
            (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) total-reward))
            
            ;; Decrease token balance
            (var-set reward-token-balance (- (var-get reward-token-balance) total-reward))
            
            ;; In a real contract, we would transfer tokens here
            ;; This would involve calling a token contract's transfer function
            
            (ok total-reward)
        )
    )
)

;; Enable or disable reward distribution (only contract owner can call)
(define-public (set-distribution-enabled (enabled bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (var-set distribution-enabled enabled)
        
        (ok enabled)
    )
)

;; Update community rating for a node (only contract owner can call)
;; In a production system, this would likely use a weighted voting mechanism
(define-public (update-community-rating (node principal) (rating uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= rating u100) ERR-INVALID-AMOUNT)  ;; Rating as percentage (0-100)
        
        (let (
            (node-data (default-to { registered: false, last-reward-epoch: u0, uptime-score: u0, response-time-score: u0, community-rating: u0 } 
                                 (map-get? nodes node)))
        )
            (asserts! (get registered node-data) ERR-INVALID-NODE)
            
            (map-set nodes node (merge node-data { community-rating: rating }))
            
            (ok true)
        )
    )
)

;; Read-only functions

;; Get current epoch
(define-read-only (get-current-epoch)
    (var-get current-epoch)
)

;; Get node data
(define-read-only (get-node-data (node principal))
    (default-to 
        { registered: false, last-reward-epoch: u0, uptime-score: u0, response-time-score: u0, community-rating: u0 } 
        (map-get? nodes node)
    )
)

;; Get verifier data
(define-read-only (get-verifier-data (verifier principal))
    (default-to 
        { registered: false, last-reward-epoch: u0, correct-validations: u0, total-validations: u0, reputation-score: u0 } 
        (map-get? verifiers verifier)
    )
)

;; Get node performance for an epoch
(define-read-only (get-node-performance (node principal) (epoch uint))
    (default-to 
        { uptime: u0, response-time: u0, verified: false } 
        (map-get? node-performance-reports { node: node, epoch: epoch })
    )
)

;; Get epoch reward data
(define-read-only (get-epoch-rewards (epoch uint))
    (default-to 
        { node-rewards-pool: u0, verifier-rewards-pool: u0, distributed: false } 
        (map-get? epoch-rewards epoch)
    )
)

;; Check if participant has claimed rewards for an epoch
(define-read-only (has-claimed-rewards (participant principal) (epoch uint))
    (default-to 
        false 
        (get claimed (map-get? claimed-rewards { participant: participant, epoch: epoch }))
    )
)

;; Get estimated rewards for a participant
(define-read-only (get-estimated-rewards (participant principal) (epoch uint))
    (let (
        (is-node (default-to false (get registered (map-get? nodes participant))))
        (is-verifier (default-to false (get registered (map-get? verifiers participant))))
        (node-reward (if is-node (calculate-node-reward participant epoch) u0))
        (verifier-reward (if is-verifier (calculate-verifier-reward participant epoch) u0))
    )
    (+ node-reward verifier-reward))
)

;; Get total rewards distributed
(define-read-only (get-total-rewards-distributed)
    (var-get total-rewards-distributed)
)

;; Get current reward token balance
(define-read-only (get-reward-token-balance)
    (var-get reward-token-balance)
)

;; Check if distribution is currently enabled
(define-read-only (is-distribution-enabled)
    (var-get distribution-enabled)
)