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
