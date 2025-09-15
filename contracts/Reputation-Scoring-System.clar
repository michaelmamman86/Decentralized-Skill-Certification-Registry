;; Reputation Scoring System
;; Tracks and calculates reputation scores for all ecosystem participants

;; Constants
(define-constant contract-owner tx-sender)
(define-constant max-reputation-score u1000)
(define-constant min-reputation-score u0)
(define-constant initial-reputation-score u500)

;; Error constants
(define-constant err-not-authorized (err u400))
(define-constant err-invalid-score (err u401))
(define-constant err-user-not-found (err u402))
(define-constant err-insufficient-data (err u403))

;; Data variables
(define-data-var reputation-update-counter uint u0)
(define-data-var total-participants uint u0)

;; Core reputation tracking for different user types
(define-map issuer-reputation
    principal
    {
        current-score: uint,
        total-certifications-issued: uint,
        certifications-revoked: uint,
        average-certification-validity: uint,
        consistency-rating: uint,
        community-trust-score: uint,
        last-updated: uint,
        reputation-trend: uint
    })

(define-map evaluator-reputation
    principal
    {
        current-score: uint,
        total-evaluations: uint,
        accuracy-score: uint,
        agreement-with-peers: uint,
        evaluation-quality: uint,
        response-timeliness: uint,
        last-updated: uint,
        expertise-breadth: uint
    })

(define-map learner-reputation
    principal
    {
        current-score: uint,
        certifications-earned: uint,
        skill-diversity: uint,
        learning-consistency: uint,
        community-engagement: uint,
        achievement-rate: uint,
        last-updated: uint,
        growth-trajectory: uint
    })

;; Reputation events and feedback tracking
(define-map reputation-events
    uint
    {
        participant: principal,
        event-type: (string-ascii 32),
        impact-score: uint,
        description: (string-ascii 128),
        timestamp: uint,
        verified: bool
    })

(define-map peer-feedback
    { reviewer: principal, reviewed: principal }
    {
        feedback-score: uint,
        collaboration-rating: uint,
        trustworthiness: uint,
        professionalism: uint,
        timestamp: uint,
        comment: (string-ascii 256)
    })

;; Reputation badges and achievements
(define-map reputation-badges
    { participant: principal, badge-type: (string-ascii 32) }
    {
        earned: bool,
        earned-date: uint,
        badge-level: uint,
        requirements-met: (list 5 bool)
    })

;; Initialize or update issuer reputation
(define-public (update-issuer-reputation 
    (issuer principal)
    (certifications-issued uint)
    (certifications-revoked uint)
    (avg-validity uint))
    (let ((current-rep (default-to 
            {
                current-score: initial-reputation-score,
                total-certifications-issued: u0,
                certifications-revoked: u0,
                average-certification-validity: u0,
                consistency-rating: u100,
                community-trust-score: u100,
                last-updated: u0,
                reputation-trend: u0
            }
            (map-get? issuer-reputation issuer)))
          (new-total (+ (get total-certifications-issued current-rep) certifications-issued))
          (new-revoked (+ (get certifications-revoked current-rep) certifications-revoked))
          (revocation-rate (if (> new-total u0) (/ (* new-revoked u100) new-total) u0))
          (quality-score (if (< revocation-rate u10) u100 (- u100 (* revocation-rate u2))))
          (consistency-factor (calculate-consistency-factor issuer))
          (new-score (calculate-issuer-score quality-score consistency-factor avg-validity)))
        
        (map-set issuer-reputation issuer
            {
                current-score: new-score,
                total-certifications-issued: new-total,
                certifications-revoked: new-revoked,
                average-certification-validity: avg-validity,
                consistency-rating: consistency-factor,
                community-trust-score: quality-score,
                last-updated: stacks-block-height,
                reputation-trend: (calculate-trend (get current-score current-rep) new-score)
            })
        
        (var-set reputation-update-counter (+ (var-get reputation-update-counter) u1))
        (ok new-score)))

;; Update evaluator reputation based on evaluation performance
(define-public (update-evaluator-reputation 
    (evaluator principal)
    (evaluations-completed uint)
    (accuracy-score uint)
    (peer-agreement uint))
    (let ((current-rep (default-to 
            {
                current-score: initial-reputation-score,
                total-evaluations: u0,
                accuracy-score: u0,
                agreement-with-peers: u0,
                evaluation-quality: u0,
                response-timeliness: u100,
                last-updated: u0,
                expertise-breadth: u0
            }
            (map-get? evaluator-reputation evaluator)))
          (new-total (+ (get total-evaluations current-rep) evaluations-completed))
          (weighted-accuracy (calculate-weighted-average 
                              (get accuracy-score current-rep) 
                              accuracy-score 
                              (get total-evaluations current-rep) 
                              evaluations-completed))
          (weighted-agreement (calculate-weighted-average 
                               (get agreement-with-peers current-rep) 
                               peer-agreement 
                               (get total-evaluations current-rep) 
                               evaluations-completed))
          (quality-score (/ (+ weighted-accuracy weighted-agreement) u2))
          (new-score (calculate-evaluator-score quality-score new-total)))
        
        (map-set evaluator-reputation evaluator
            {
                current-score: new-score,
                total-evaluations: new-total,
                accuracy-score: weighted-accuracy,
                agreement-with-peers: weighted-agreement,
                evaluation-quality: quality-score,
                response-timeliness: (get response-timeliness current-rep),
                last-updated: stacks-block-height,
                expertise-breadth: (calculate-expertise-breadth evaluator)
            })
        
        (ok new-score)))

;; Update learner reputation based on learning achievements
(define-public (update-learner-reputation 
    (learner principal)
    (new-certifications uint)
    (skill-areas-count uint)
    (achievement-rate uint))
    (let ((current-rep (default-to 
            {
                current-score: initial-reputation-score,
                certifications-earned: u0,
                skill-diversity: u0,
                learning-consistency: u100,
                community-engagement: u50,
                achievement-rate: u0,
                last-updated: u0,
                growth-trajectory: u0
            }
            (map-get? learner-reputation learner)))
          (new-total-certs (+ (get certifications-earned current-rep) new-certifications))
          (new-diversity (max-value (get skill-diversity current-rep) skill-areas-count))
          (engagement-score (calculate-engagement-score learner))
          (consistency-score (calculate-learning-consistency learner))
          (new-score (calculate-learner-score new-total-certs new-diversity achievement-rate engagement-score)))
        
        (map-set learner-reputation learner
            {
                current-score: new-score,
                certifications-earned: new-total-certs,
                skill-diversity: new-diversity,
                learning-consistency: consistency-score,
                community-engagement: engagement-score,
                achievement-rate: achievement-rate,
                last-updated: stacks-block-height,
                growth-trajectory: (calculate-trend (get current-score current-rep) new-score)
            })
        
        (ok new-score)))

;; Submit peer feedback about another participant
(define-public (submit-peer-feedback 
    (reviewed principal)
    (feedback-score uint)
    (collaboration-rating uint)
    (trustworthiness uint)
    (professionalism uint)
    (comment (string-ascii 256)))
    (begin
        (asserts! (<= feedback-score u100) err-invalid-score)
        (asserts! (<= collaboration-rating u100) err-invalid-score)
        (asserts! (<= trustworthiness u100) err-invalid-score)
        (asserts! (<= professionalism u100) err-invalid-score)
        
        (map-set peer-feedback { reviewer: tx-sender, reviewed: reviewed }
            {
                feedback-score: feedback-score,
                collaboration-rating: collaboration-rating,
                trustworthiness: trustworthiness,
                professionalism: professionalism,
                timestamp: stacks-block-height,
                comment: comment
            })
        
        (ok true)))

;; Award reputation badge based on achievements
(define-public (award-reputation-badge 
    (participant principal)
    (badge-type (string-ascii 32))
    (badge-level uint)
    (requirements-met (list 5 bool)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        
        (map-set reputation-badges { participant: participant, badge-type: badge-type }
            {
                earned: true,
                earned-date: stacks-block-height,
                badge-level: badge-level,
                requirements-met: requirements-met
            })
        
        (ok true)))

;; Helper functions for reputation calculations
(define-private (min-value (a uint) (b uint))
    (if (< a b) a b))

(define-private (max-value (a uint) (b uint))
    (if (> a b) a b))

(define-private (calculate-issuer-score (quality uint) (consistency uint) (validity uint))
    (let ((weighted-score (/ (+ (* quality u4) (* consistency u3) (* validity u3)) u10)))
        (min-value max-reputation-score (max-value min-reputation-score weighted-score))))

(define-private (calculate-evaluator-score (quality uint) (total-evals uint))
    (let ((experience-bonus (if (> total-evals u50) u50 (/ total-evals u1)))
          (base-score (+ quality experience-bonus)))
        (min-value max-reputation-score (max-value min-reputation-score base-score))))

(define-private (calculate-learner-score (certs uint) (diversity uint) (achievement uint) (engagement uint))
    (let ((cert-bonus (if (> certs u10) u100 (* certs u10)))
          (diversity-bonus (* diversity u20))
          (weighted-score (/ (+ cert-bonus diversity-bonus achievement engagement) u4)))
        (min-value max-reputation-score (max-value min-reputation-score weighted-score))))

(define-private (calculate-weighted-average (old-val uint) (new-val uint) (old-count uint) (new-count uint))
    (if (> (+ old-count new-count) u0)
        (/ (+ (* old-val old-count) (* new-val new-count)) (+ old-count new-count))
        u0))

(define-private (calculate-trend (old-score uint) (new-score uint))
    (if (> new-score old-score)
        (- new-score old-score)
        u0))

(define-private (calculate-consistency-factor (user principal))
    u100)

(define-private (calculate-engagement-score (user principal))
    u75)

(define-private (calculate-learning-consistency (user principal))
    u85)

(define-private (calculate-expertise-breadth (evaluator principal))
    u3)

;; Read-only functions
(define-read-only (get-issuer-reputation (issuer principal))
    (map-get? issuer-reputation issuer))

(define-read-only (get-evaluator-reputation (evaluator principal))
    (map-get? evaluator-reputation evaluator))

(define-read-only (get-learner-reputation (learner principal))
    (map-get? learner-reputation learner))

(define-read-only (get-overall-reputation (participant principal))
    (let ((issuer-rep (default-to { current-score: u0 } (map-get? issuer-reputation participant)))
          (evaluator-rep (default-to { current-score: u0 } (map-get? evaluator-reputation participant)))
          (learner-rep (default-to { current-score: u0 } (map-get? learner-reputation participant))))
        (/ (+ (get current-score issuer-rep) 
              (get current-score evaluator-rep) 
              (get current-score learner-rep)) u3)))

(define-read-only (get-reputation-badge (participant principal) (badge-type (string-ascii 32)))
    (map-get? reputation-badges { participant: participant, badge-type: badge-type }))

(define-read-only (get-peer-feedback (reviewer principal) (reviewed principal))
    (map-get? peer-feedback { reviewer: reviewer, reviewed: reviewed }))

(define-read-only (calculate-trust-score (participant principal))
    (let ((overall-rep (get-overall-reputation participant))
          (feedback-count u5)
          (trust-modifier (if (> feedback-count u3) u10 u0)))
        (+ overall-rep trust-modifier)))
