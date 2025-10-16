;; Performance Analysis Engine
;; Handles musician performance data and feedback

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-score (err u101))
(define-constant err-not-registered (err u102))
(define-constant err-already-registered (err u103))

;; Data Variables
(define-data-var min-acceptable-score uint u60)
(define-data-var max-sessions-per-day uint u10)

;; Data Maps
(define-map musicians 
    principal 
    {
        registered: bool,
        total-sessions: uint,
        average-score: uint,
        best-score: uint,
        last-session: uint
    }
)

(define-map performance-sessions 
    uint 
    {
        musician: principal,
        timestamp: uint,
        pitch-score: uint,
        rhythm-score: uint,
        tone-score: uint,
        overall-score: uint,
        feedback: (string-ascii 256)
    }
)

(define-data-var session-counter uint u0)

;; Private Functions
(define-private (calculate-overall-score 
    (pitch uint) 
    (rhythm uint) 
    (tone uint)
    )
    (begin
        (asserts! (and (< pitch u101) (< rhythm u101) (< tone u101)) err-invalid-score)
        (ok (/ (+ pitch rhythm tone) u3))
    )
)

(define-private (generate-feedback 
    (overall-score uint)
    )
    (if (< overall-score (var-get min-acceptable-score))
        "Need improvement. Focus on fundamentals."
        (if (< overall-score u80)
            "Good progress. Keep practicing consistently."
            (if (< overall-score u90)
                "Excellent work! Fine-tune your technique."
                "Outstanding performance! Consider advanced material."
            )
        )
    )
)

(define-private (update-musician-stats 
    (musician principal) 
    (new-score uint)
    )
    (let (
        (current-stats (unwrap! (map-get? musicians musician) err-not-registered))
        (new-total (+ (get total-sessions current-stats) u1))
        (new-average (/ (+ (* (get average-score current-stats) 
                             (get total-sessions current-stats)) 
                          new-score) 
                       new-total))
        )
        (map-set musicians 
            musician
            {
                registered: true,
                total-sessions: new-total,
                average-score: new-average,
                best-score: (if (> new-score (get best-score current-stats))
                              new-score
                              (get best-score current-stats)),
                last-session: burn-block-height
            }
        )
        (ok true)
    )
)

;; Public Functions
(define-public (register-musician)
    (begin
        (asserts! (is-none (map-get? musicians tx-sender)) err-already-registered)
        (map-set musicians 
            tx-sender
            {
                registered: true,
                total-sessions: u0,
                average-score: u0,
                best-score: u0,
                last-session: u0
            }
        )
        (ok true)
    )
)

(define-public (submit-performance 
    (pitch-score uint) 
    (rhythm-score uint) 
    (tone-score uint)
    )
    (let (
        (overall-score (unwrap! (calculate-overall-score pitch-score rhythm-score tone-score) err-invalid-score))
        (feedback (generate-feedback overall-score))
        (session-id (+ (var-get session-counter) u1))
        )
        (asserts! (is-some (map-get? musicians tx-sender)) err-not-registered)
        (var-set session-counter session-id)
        (map-set performance-sessions
            session-id
            {
                musician: tx-sender,
                timestamp: burn-block-height,
                pitch-score: pitch-score,
                rhythm-score: rhythm-score,
                tone-score: tone-score,
                overall-score: overall-score,
                feedback: feedback
            }
        )
        (update-musician-stats tx-sender overall-score)
    )
)

(define-read-only (get-musician-stats (musician principal))
    (ok (unwrap! (map-get? musicians musician) err-not-registered))
)

(define-read-only (get-session-details (session-id uint))
    (ok (unwrap! (map-get? performance-sessions session-id) err-not-registered))
)

;; Admin Functions
(define-public (update-min-score (new-min uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (< new-min u101) err-invalid-score)
        (var-set min-acceptable-score new-min)
        (ok true)
    )
)

(define-public (update-max-sessions (new-max uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set max-sessions-per-day new-max)
        (ok true)
    )
)