;; Skill Certification Registry Contract
;; Enables issuance and verification of educational credentials

;; Contract owner

(define-non-fungible-token certification uint)

;; Data structures
(define-data-var cert-counter uint u0)

(define-map certification-details
    uint
    {
        recipient: principal,
        issuer: principal,
        skill: (string-ascii 64),
        issue-date: uint,
        expiry-date: uint,
        revoked: bool,
        metadata: (string-ascii 256),
        level: uint
    }
)

(define-map authorized-issuers principal bool)

(define-data-var contract-owner principal tx-sender)
;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-CERTIFICATION (err u101))
(define-constant ERR-CERTIFICATION-REVOKED (err u102))
(define-constant ERR-CERTIFICATION-EXPIRED (err u103))

;; Contract owner only functions
(define-public (add-authorized-issuer (issuer principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set authorized-issuers issuer true))
    )
)

(define-public (remove-authorized-issuer (issuer principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set authorized-issuers issuer false))
    )
)

;; Issue new certification
(define-public (issue-certification 
    (recipient principal)
    (skill (string-ascii 64))
    (expiry-date uint)
    (metadata (string-ascii 256)))
    (let
        (
            (cert-id (var-get cert-counter))
            (issuer tx-sender)
        )
        ;; Check if issuer is authorized
        (asserts! (default-to false (map-get? authorized-issuers issuer)) ERR-NOT-AUTHORIZED)
        
        ;; Increment counter
        (var-set cert-counter (+ cert-id u1))
        
        ;; Mint NFT
        (try! (nft-mint? certification cert-id recipient))
        
        ;; Store certification details
        (map-set certification-details cert-id {
            recipient: recipient,
            issuer: issuer,
            skill: skill,
            issue-date: stacks-block-height,
            expiry-date: expiry-date,
            revoked: false,
            metadata: metadata,
            level: u1
        })
        
        (ok cert-id)
    )
)

;; Revoke certification
(define-public (revoke-certification (cert-id uint))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION)))
        
        ;; Check if caller is the issuer
        (asserts! (is-eq tx-sender (get issuer cert-info)) ERR-NOT-AUTHORIZED)
        
        ;; Update certification details
        (map-set certification-details cert-id 
            (merge cert-info { revoked: true })
        )
        
        (ok true)
    )
)

;; Read-only functions for verification
(define-read-only (verify-certification (cert-id uint))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION)))
        
        ;; Check if certification is not revoked
        (asserts! (not (get revoked cert-info)) ERR-CERTIFICATION-REVOKED)
        
        ;; Check if certification is not expired
        (asserts! (< stacks-block-height (get expiry-date cert-info)) ERR-CERTIFICATION-EXPIRED)
        
        (ok cert-info)
    )
)

(define-read-only (get-certification-details (cert-id uint))
    (map-get? certification-details cert-id)
)

(define-read-only (is-authorized-issuer (issuer principal))
    (default-to false (map-get? authorized-issuers issuer))
)



;; Allow transfer of certifications between principals
(define-public (transfer-certification (cert-id uint) (recipient principal))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION)))
        
        ;; Check if caller owns the certification
        (asserts! (is-eq tx-sender (get recipient cert-info)) ERR-NOT-AUTHORIZED)
        
        ;; Transfer NFT
        (try! (nft-transfer? certification cert-id tx-sender recipient))
        
        ;; Update certification details
        (map-set certification-details cert-id 
            (merge cert-info { recipient: recipient })
        )
        
        (ok true)
    )
)



(define-private (issue-single-certification  (recipient principal) (skill (string-ascii 64)) (expiry-date uint) (metadata (string-ascii 256)))
    (let
        (
            (cert-id (var-get cert-counter))
        )
        ;; Increment counter
        (var-set cert-counter (+ cert-id u1))
        
        ;; Mint NFT
        (try! (nft-mint? certification cert-id recipient))
        
        ;; Store certification details
        (map-set certification-details cert-id {
            recipient: recipient,
            issuer: tx-sender,
            skill: skill,
            issue-date: stacks-block-height,
            expiry-date: expiry-date,
            revoked: false,
            metadata: metadata,
            level: u1

        })
        
        (ok cert-id)
    )
)

(define-private (process-single-recipient (recipient principal) (skill (string-ascii 64)) (expiry-date uint) (metadata (string-ascii 256)))
    (issue-single-certification recipient skill expiry-date metadata))


(define-public (renew-certification 
    (cert-id uint)
    (new-expiry-date uint))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION)))
        
        ;; Check if caller is the issuer
        (asserts! (is-eq tx-sender (get issuer cert-info)) ERR-NOT-AUTHORIZED)
        
        ;; Update certification details
        (map-set certification-details cert-id 
            (merge cert-info { expiry-date: new-expiry-date })
        )
        
        (ok true)
    )
)




(define-map certification-ratings
    { cert-id: uint, rater: principal }
    { rating: uint, comment: (string-ascii 256) }
)

(define-public (rate-certification 
    (cert-id uint)
    (rating uint)
    (comment (string-ascii 256)))
    (begin
        (asserts! (<= rating u5) (err u104)) ;; Rating must be 1-5
        (ok (map-set certification-ratings 
            { cert-id: cert-id, rater: tx-sender }
            { rating: rating, comment: comment }
        ))
    )
)




(define-constant LEVEL-BEGINNER u1)
(define-constant LEVEL-INTERMEDIATE u2)
(define-constant LEVEL-EXPERT u3)

(define-public (update-certification-level 
    (cert-id uint)
    (new-level uint))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION)))
        
        (asserts! (is-eq tx-sender (get issuer cert-info)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-level LEVEL-EXPERT) (err u105))
        
        (ok (map-set certification-details cert-id 
            (merge cert-info { level: new-level })
        ))
    )
)



(define-map certification-endorsements
    { cert-id: uint, endorser: principal }
    { endorsed: bool, timestamp: uint }
)

(define-public (endorse-certification (cert-id uint))
    (begin
        (asserts! (is-authorized-issuer tx-sender) ERR-NOT-AUTHORIZED)
        (ok (map-set certification-endorsements 
            { cert-id: cert-id, endorser: tx-sender }
            { endorsed: true, timestamp: stacks-block-height }
        ))
    )
)




(define-map certification-prerequisites
    uint
    (list 10 uint)
)

(define-public (set-certification-prerequisites 
    (cert-id uint)
    (prerequisite-ids (list 10 uint)))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION)))
        
        (asserts! (is-eq tx-sender (get issuer cert-info)) ERR-NOT-AUTHORIZED)
        
        (ok (map-set certification-prerequisites 
            cert-id
            prerequisite-ids
        ))
    )
)



;; Add these data structures and functions

(define-map verification-history
    { cert-id: uint, verifier: principal }
    { timestamp: uint, count: uint }
)

(define-public (verify-and-log-certification (cert-id uint))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION))
         (existing-record (default-to { timestamp: u0, count: u0 } 
                          (map-get? verification-history { cert-id: cert-id, verifier: tx-sender })))
         (new-count (+ (get count existing-record) u1)))
        
        ;; Check if certification is not revoked
        (asserts! (not (get revoked cert-info)) ERR-CERTIFICATION-REVOKED)
        
        ;; Check if certification is not expired
        (asserts! (< stacks-block-height (get expiry-date cert-info)) ERR-CERTIFICATION-EXPIRED)
        
        ;; Update verification history
        (map-set verification-history 
            { cert-id: cert-id, verifier: tx-sender }
            { timestamp: stacks-block-height, count: new-count }
        )
        
        (ok cert-info)
    )
)

(define-read-only (get-verification-history (cert-id uint) (verifier principal))
    (map-get? verification-history { cert-id: cert-id, verifier: verifier })
)


;; Add these data structures and functions

(define-map certification-categories
    uint
    { category: (string-ascii 64), tags: (list 5 (string-ascii 32)) }
)

(define-public (add-certification-category-and-tags 
    (cert-id uint)
    (category (string-ascii 64))
    (tags (list 5 (string-ascii 32))))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION)))
        
        ;; Check if caller is the issuer
        (asserts! (is-eq tx-sender (get issuer cert-info)) ERR-NOT-AUTHORIZED)
        
        ;; Set category and tags
        (ok (map-set certification-categories 
            cert-id
            { category: category, tags: tags }
        ))
    )
)

(define-read-only (get-certification-category-and-tags (cert-id uint))
    (map-get? certification-categories cert-id)
)


;; Add these data structures and functions

(define-map delegation-details
    principal
    { delegator: principal, expiry: uint, active: bool }
)

(define-constant ERR-DELEGATION-EXPIRED (err u106))
(define-constant ERR-NOT-DELEGATED (err u107))

(define-public (delegate-certification-authority 
    (delegate principal)
    (expiry-blocks uint))
    (begin
        ;; Check if caller is authorized
        (asserts! (default-to false (map-get? authorized-issuers tx-sender)) ERR-NOT-AUTHORIZED)
        
        ;; Set delegation
        (ok (map-set delegation-details 
            delegate
            { delegator: tx-sender, expiry: (+ stacks-block-height expiry-blocks), active: true }
        ))
    )
)

(define-public (revoke-delegation (delegate principal))
    (let
        ((delegation (unwrap! (map-get? delegation-details delegate) ERR-NOT-DELEGATED)))
        
        ;; Check if caller is the delegator
        (asserts! (is-eq tx-sender (get delegator delegation)) ERR-NOT-AUTHORIZED)
        
        ;; Revoke delegation
        (ok (map-set delegation-details 
            delegate
            (merge delegation { active: false })
        ))
    )
)

(define-read-only (is-valid-delegate (delegate principal))
    (let
        ((delegation (default-to { delegator: delegate, expiry: u0, active: false } 
                     (map-get? delegation-details delegate))))
        
        (and 
            (get active delegation)
            (< stacks-block-height (get expiry delegation))
            (default-to false (map-get? authorized-issuers (get delegator delegation)))
        )
    )
)

;; Modify the issue-certification function to check for delegation
(define-public (issue-certification-as-delegate 
    (recipient principal)
    (skill (string-ascii 64))
    (expiry-date uint)
    (metadata (string-ascii 256)))
    (let
        (
            (cert-id (var-get cert-counter))
            (issuer tx-sender)
            (delegation (default-to { delegator: issuer, expiry: u0, active: false } 
                        (map-get? delegation-details issuer)))
        )
        ;; Check if issuer is a valid delegate
        (asserts! (is-valid-delegate issuer) ERR-NOT-AUTHORIZED)
        
        ;; Increment counter
        (var-set cert-counter (+ cert-id u1))
        
        ;; Mint NFT
        (try! (nft-mint? certification cert-id recipient))
        
        ;; Store certification details with the delegator as issuer
        (map-set certification-details cert-id {
            recipient: recipient,
            issuer: (get delegator delegation),  ;; The original delegator is recorded as issuer
            skill: skill,
            issue-date: stacks-block-height,
            expiry-date: expiry-date,
            revoked: false,
            metadata: metadata,
            level: u1
        })
        
        (ok cert-id)
    )
)



(define-public (renew-certification-as-delegate 
    (cert-id uint)
    (new-expiry-date uint))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION))
         (delegation (default-to { delegator: tx-sender, expiry: u0, active: false } 
                        (map-get? delegation-details tx-sender))))
        
        ;; Check if issuer is a valid delegate
        (asserts! (is-valid-delegate tx-sender) ERR-NOT-AUTHORIZED)
        
        ;; Update certification details
        (map-set certification-details cert-id 
            (merge cert-info { expiry-date: new-expiry-date })
        )
        
        (ok true)
    )
)


;; Add these data structures and functions

(define-map certification-upgrade-paths
    uint  ;; source certification ID
    { target-cert-id: uint, requirements: (string-ascii 256) }
)

(define-public (set-certification-upgrade-path 
    (source-cert-id uint)
    (target-cert-id uint)
    (requirements (string-ascii 256)))
    (let
        ((source-cert (unwrap! (map-get? certification-details source-cert-id) ERR-INVALID-CERTIFICATION))
         (target-cert (unwrap! (map-get? certification-details target-cert-id) ERR-INVALID-CERTIFICATION)))
        
        ;; Check if caller is the issuer of both certifications
        (asserts! (and (is-eq tx-sender (get issuer source-cert)) 
                       (is-eq tx-sender (get issuer target-cert))) 
                  ERR-NOT-AUTHORIZED)
        
        ;; Set upgrade path
        (ok (map-set certification-upgrade-paths 
            source-cert-id
            { target-cert-id: target-cert-id, requirements: requirements }
        ))
    )
)

(define-read-only (get-certification-upgrade-path (source-cert-id uint))
    (map-get? certification-upgrade-paths source-cert-id)
)



(define-public (upgrade-certification 
    (source-cert-id uint)
    (target-cert-id uint))
    (let
        ((source-cert (unwrap! (map-get? certification-details source-cert-id) ERR-INVALID-CERTIFICATION))
         (target-cert (unwrap! (map-get? certification-details target-cert-id) ERR-INVALID-CERTIFICATION))
         (upgrade-path (unwrap! (map-get? certification-upgrade-paths source-cert-id) ERR-INVALID-CERTIFICATION)))
        
        ;; Check if caller is the issuer of the source certification
        (asserts! (is-eq tx-sender (get issuer source-cert)) ERR-NOT-AUTHORIZED)
        
        ;; Check if target certification is the expected target
        (asserts! (is-eq target-cert-id (get target-cert-id upgrade-path)) ERR-INVALID-CERTIFICATION)
        
        (ok true)
    )
)

;; Add these data structures and functions

(define-map certification-achievements
    { cert-id: uint, achievement-id: uint }
    { title: (string-ascii 64), description: (string-ascii 256), date-achieved: uint }
)

(define-data-var achievement-counter uint u0)

(define-public (add-certification-achievement 
    (cert-id uint)
    (title (string-ascii 64))
    (description (string-ascii 256)))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION))
         (achievement-id (var-get achievement-counter)))
        
        ;; Check if caller is the issuer
        (asserts! (is-eq tx-sender (get issuer cert-info)) ERR-NOT-AUTHORIZED)
        
        ;; Increment achievement counter
        (var-set achievement-counter (+ achievement-id u1))
        
        ;; Add achievement
        (ok (map-set certification-achievements 
            { cert-id: cert-id, achievement-id: achievement-id }
            { title: title, description: description, date-achieved: stacks-block-height }
        ))
    )
)

(define-read-only (get-certification-achievement (cert-id uint) (achievement-id uint))
    (map-get? certification-achievements { cert-id: cert-id, achievement-id: achievement-id })
)


;; Add these functions

(define-read-only (get-expiring-certifications (issuer principal) (blocks-threshold uint))
    (let
        ((expiry-threshold (+ stacks-block-height blocks-threshold)))
        
        ;; This is a read-only function that would ideally return all certifications
        ;; that will expire within the threshold. Since we can't iterate through maps
        ;; in Clarity, this is a placeholder for the concept.
        ;; In practice, you would need to track certifications by issuer in a separate map
        ;; or implement this logic in the frontend.
        
        expiry-threshold  ;; Return the threshold for frontend processing
    )
)

(define-map expiration-notification-settings
    principal
    { enabled: bool, threshold-blocks: uint }
)

(define-public (set-expiration-notification-settings 
    (enabled bool)
    (threshold-blocks uint))
    (ok (map-set expiration-notification-settings 
        tx-sender
        { enabled: enabled, threshold-blocks: threshold-blocks }
    ))
)

(define-read-only (get-expiration-notification-settings (user principal))
    (default-to { enabled: false, threshold-blocks: u1000 } 
              (map-get? expiration-notification-settings user))
)


;; Add these data structures and functions

(define-map certification-disputes
    uint  ;; cert-id
    { 
        disputed: bool, 
        reason: (string-ascii 256), 
        disputant: principal,
        issuer-response: (string-ascii 256),
        status: (string-ascii 32),  ;; "pending", "resolved", "rejected"
        timestamp: uint
    }
)

(define-constant ERR-ALREADY-DISPUTED (err u108))
(define-constant ERR-NOT-RECIPIENT (err u109))
(define-constant ERR-NO-DISPUTE (err u110))

(define-public (file-certification-dispute 
    (cert-id uint)
    (reason (string-ascii 256)))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION))
         (existing-dispute (map-get? certification-disputes cert-id)))
        
        ;; Check if caller is the recipient
        (asserts! (is-eq tx-sender (get recipient cert-info)) ERR-NOT-RECIPIENT)
        
        ;; Check if not already disputed
        (asserts! (is-none existing-dispute) ERR-ALREADY-DISPUTED)
        
        ;; File dispute
        (ok (map-set certification-disputes 
            cert-id
            { 
                disputed: true, 
                reason: reason, 
                disputant: tx-sender,
                issuer-response: "",
                status: "pending",
                timestamp: stacks-block-height
            }
        ))
    )
)

(define-public (respond-to-dispute 
    (cert-id uint)
    (response (string-ascii 256))
    (new-status (string-ascii 32)))
    (let
        ((cert-info (unwrap! (map-get? certification-details cert-id) ERR-INVALID-CERTIFICATION))
         (dispute (unwrap! (map-get? certification-disputes cert-id) ERR-NO-DISPUTE)))
        
        ;; Check if caller is the issuer
        (asserts! (is-eq tx-sender (get issuer cert-info)) ERR-NOT-AUTHORIZED)
        
        ;; Update dispute
        (ok (map-set certification-disputes 
            cert-id
            (merge dispute { 
                issuer-response: response,
                status: new-status
            })
        ))
    )
)

(define-read-only (get-certification-dispute (cert-id uint))
    (map-get? certification-disputes cert-id)
)
