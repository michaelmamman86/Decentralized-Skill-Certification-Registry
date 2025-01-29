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
        metadata: (string-ascii 256)
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
            metadata: metadata
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