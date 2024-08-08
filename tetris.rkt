#lang racket/base


;; -------------------------------
;; Constants

(define MS/DROP 1000)               ; ms/cell dropped due to gravity
(define MS/AUTOSHIFT 25)            ; ms/cell moved during autoshift
(define AUTOSHIFT-DELAY 133)        ; ms before autoshift starts
(define MS/SOFT-DROP 20)            ; ms/cell dropped during soft-drop


;; -------------------------------
;; Provides


(require racket/contract)
(provide
 (contract-out
  [tetris? (-> any/c boolean?)]
  [new-tetris (->* (natural-number/c)
                   (#:tetrion tetrion?
                    #:rows natural-number/c
                    #:cols natural-number/c)
                   tetris?)]
  [tetris-tn (-> tetris? tetrion?)]
  [tetris-fps (-> tetris? number?)]
  
  ;; Player actions
  [tetris-left-pressed (-> tetris? natural-number/c tetris?)]
  [tetris-left-released (-> tetris? natural-number/c tetris?)]
  [tetris-right-pressed (-> tetris? natural-number/c tetris?)]
  [tetris-right-released (-> tetris? natural-number/c tetris?)]
  [tetris-soft-drop-pressed (-> tetris? natural-number/c tetris?)]
  [tetris-soft-drop-released (-> tetris? natural-number/c tetris?)]
  [tetris-rotate-cw (-> tetris? natural-number/c tetris?)]
  [tetris-rotate-ccw (-> tetris? natural-number/c tetris?)]
  [tetris-rotate-180 (-> tetris? natural-number/c tetris?)]  
  [tetris-hard-drop (-> tetris? natural-number/c tetris?)]

  ;; Big bang on-tick
  [tetris-on-tick (-> tetris? natural-number/c tetris?)]
  ))


; -------------------------------
; Requires


(require threading)
(require "tetrion.rkt")
(require "playfield.rkt")
(require "block.rkt")
(require "utils.rkt")


;; ----------------------------
;; Definitions


;; A Tetris is a struct:
;;   - tn: Tetrion
;;   - pressed-hash: hash mapping a key to a (cons Boolean Natural],
;;                   whether it is pressed or not, and the timestamp when it was last pressed/released
;;   - t-drop: timer for gravity drop, is set to the timestamp of the last drop or spawn
;;   - t-autoshift: timer for autoshift move, set when autoshft last moved the piece
;;   - ticks: queue of the timestamps of the last few ticks (to compute the FPS)
(struct tetris [tn
                pressed-hash
                t-drop
                t-autoshift
                ticks])


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(define (new-tetris ms
                    #:rows [rows 20]
                    #:cols [cols 10]
                    #:tetrion [tetrion (~> (new-tetrion #:rows rows #:cols cols)
                                           tetrion-spawn)])
  (~> (tetris tetrion (hash) ms 0 '(0 0 0 0 0))))


;; Set the state and time of the last key press/release
(define (tetris--set-pressed t k ms pressed?)
  (define h (tetris-pressed-hash t))
  (define new-h (hash-set h k (cons pressed? ms)))
  (struct-copy tetris t [pressed-hash new-h]))

;; Return #t if key is pressed, otherwise #f
(define (tetris--pressed? t k)
  (define h (tetris-pressed-hash t))
  (car (hash-ref h k (cons #f 0))))

;; Return the time when key was last pressed/released
(define (tetris--pressed-t t k)
  (define h (tetris-pressed-hash t))
  (cdr (hash-ref h k (cons #f 0))))


;; call tetrion-move and update the lock timer if successful move
(define (tetris--move t ms dirn)
  (define tn (tetris-tn t))
  (define new-tn
    (with-handlers ([exn:fail? (λ (e) tn)])
      (if (equal? dirn 'right)
          (tetrion-right tn)
          (tetrion-left tn))))
  (struct-copy tetris t [tn new-tn]))


; dirn is either 'left or 'right
(define (tetris--dirn-pressed t ms dirn)
  (~> t
      (tetris--move ms dirn)
      (tetris--set-pressed dirn ms #t)))


(define (tetris-right-pressed t ms)
  (tetris--dirn-pressed t ms 'right))

(define (tetris-left-pressed t ms)
  (tetris--dirn-pressed t ms 'left))

(define (tetris-soft-drop-pressed t ms)
  (~> t
      (tetris--set-pressed 'down ms #t)
      ;; restart the drop timer so it doesn't drop too much at the start
      ;; and subtract one time unit so it drops 1 block immediately
      (struct-copy tetris _ [t-drop (- ms MS/SOFT-DROP)])))

(define (tetris-right-released t ms)
  (tetris--set-pressed t 'right ms #f))

(define (tetris-left-released t ms)
  (tetris--set-pressed t 'left ms #f))

(define (tetris-soft-drop-released t ms)
  (tetris--set-pressed t 'down ms #f))


;; Tetris -> Tetris
;; simply drop the piece,
 ;; and update the t-on-ground
(define (tetris--drop t)
  (define tn (tetris-tn t))
  (define new-tn
    (with-handlers ([exn:fail? (λ (e) tn)])
      (tetrion-drop tn)))
  (struct-copy tetris t
               [tn new-tn]))


;; Apply gravity or soft-drop if it's time to and "down" is being held.
(define (tetris-gravity-tick t ms)
  (define drop-rate (if (tetris--pressed? t'down)
                        MS/SOFT-DROP
                        MS/DROP))
  
  (define t-drop (tetris-t-drop t))
  (define times-to-drop (quotient (- ms t-drop) drop-rate))
  (define new-t-drop (+ t-drop (* times-to-drop drop-rate)))
  (define (drop-n-times t n)
    (for/fold ([t-acc t])
              ([_ (in-range n)])
      (tetris--drop t-acc)))
  (~> t
      (drop-n-times times-to-drop)
      (struct-copy tetris _
                   [t-drop new-t-drop])))


(module+ test
  (test-case
      "Gravity drop on tick"
    (define t0
      (new-tetris 0 #:tetrion (~> (new-tetrion #:rows 2)
                                  (tetrion-spawn-shape 'L))))

    ;; Assert that we got the expected shape
    (check block-lists=?
           (~> t0 tetris-tn tetrion-playfield playfield-blocks)
           (strings->blocks '(".....L...."
                              "...LLL...."
                              ".........."
                              "..........")))

    ;; Gravity drop one row
    (define t1 (tetris-gravity-tick t0 (+ 2 MS/DROP)))
    (check block-lists=?
           (~> t1 tetris-tn tetrion-playfield playfield-blocks)
           (strings->blocks '(".........."
                              ".....L...."
                              "...LLL...."
                              "..........")))
    (check-equal? (tetris-t-drop t1) MS/DROP)

    ;; Gravity drop two rows in a tick
    (define t2 (tetris-gravity-tick t0
                                    (+ 1 (inexact->exact (* 2 MS/DROP)))))
    (check
     block-lists=?
     (~> t2 tetris-tn tetrion-playfield playfield-blocks)
     (strings->blocks '(".........."
                        ".........."
                        ".....L...."
                        "...LLL....")))
    (check-equal? (tetris-t-drop t2) (* 2 MS/DROP))

    ;; Don't gravity drop when not enough time has passed
    (define t3 (tetris-gravity-tick t0 (inexact->exact (* 0.5 MS/DROP))))
    (check
     block-lists=?
     (~> t3 tetris-tn tetrion-playfield playfield-blocks)
     (strings->blocks '(".....L...."
                        "...LLL...."
                        ".........."
                        "..........")))
    (check-equal? (tetris-t-drop t3) 0))
  )


(define (tetris-spawn t ms)
  (define new-tn (tetrion-spawn (tetris-tn t)))
  (struct-copy tetris t [tn new-tn] [t-drop ms]))


(define (tetris-lock t ms)
  (define tn-locked (~> (tetris-tn t)
                        tetrion-lock))
  (~> (struct-copy tetris t [tn tn-locked])
      (tetris-spawn ms)))


(define (tetris-hard-drop t ms)
  (define tn-dropped (~> (tetris-tn t)
                         tetrion-hard-drop))
  (~> (struct-copy tetris t [tn tn-dropped])
      (tetris-lock ms)))


(define (tetris--rotate t cw? ms)
  (define new-tn
    (with-handlers ([exn:fail? (λ (e) (tetris-tn t))])
      (tetrion-rotate (tetris-tn t) cw?)))
  (struct-copy tetris t [tn new-tn]))


(define (tetris-rotate-cw t ms)
  (tetris--rotate t #t ms))

(define (tetris-rotate-ccw t ms)
  (tetris--rotate t #f ms))


(define (tetris-rotate-180 t ms)
  (define new-tn
    (with-handlers ([exn:fail? (λ (e) (tetris-tn t))])
      (tetrion-rotate-180 (tetris-tn t))))
  (struct-copy tetris t [tn new-tn]))


;; Tetris Natural -> Tetris
;; Start autoshift if enough time has passed since holding a direction key
(define (tetris-autoshift-tick t ms)
  (define (tetris--move-n-times n dirn)
    (for/fold ([t-acc t])
              ([_ (in-range n)])
      (tetris--move t-acc ms dirn)))
  ;; Time when a dirn key was last pressed/released
  (define t-dirn (max (tetris--pressed-t t 'right) (tetris--pressed-t t 'left)))

  ;; Assuming the delay has passed,
  ;; autoshift in the given dirn if enough time has passed since the previous autoshift
  (define (autoshift-in-dirn dirn)
    ;; timestamp of previous autoshift, or of the autoshift start,
    ;; whichever happened later
    (define t-autoshift (max (tetris-t-autoshift t) (+ t-dirn AUTOSHIFT-DELAY)))
    (define t-since-autoshift (- ms t-autoshift))
    (define times-to-move (quotient t-since-autoshift MS/AUTOSHIFT))
    (~> (tetris--move-n-times times-to-move dirn)
        (struct-copy tetris _
                     [t-autoshift (+ t-autoshift (* times-to-move MS/AUTOSHIFT))])))

  (define right-pressed (tetris--pressed? t 'right))
  (define left-pressed (tetris--pressed? t 'left))
  (define delay-passed (> (- ms t-dirn) AUTOSHIFT-DELAY))
  (define right-event-last (> (tetris--pressed-t t 'right) (tetris--pressed-t t 'left)))
  (define holding-right (or (not left-pressed) (and right-pressed right-event-last)))
  (cond
    [(not (or right-pressed left-pressed)) t]
    [(not delay-passed) t]
    [holding-right (autoshift-in-dirn 'right)]
    [else (autoshift-in-dirn 'left)]))


;; Tetris Natural -> Tetris
;; Update the ticks timestamps list
(define (tetris-fps-tick t ms)
  (define ticks (tetris-ticks t))
  (define new-ticks (append (cdr ticks) (list ms)))
  (struct-copy tetris t
               [ticks new-ticks]))


;; Tetris -> Natural
;; Returns the FPS based on the last few ticks
(define (tetris-fps t)
  (define ticks (tetris-ticks t))
  ;; list of delta times between ticks
  (define dt-ls (for/sum ([t1 ticks]
                          [t2 (cdr ticks)])
                  (- t2 t1)))
  (define dt-avg (/ (exact->inexact dt-ls) (sub1 (length ticks))))
  ;; convert average ms to FPS
  (/ 1000.0 dt-avg))



;; Tetris Natural -> Tetris
;; Do everything that needs to happen on a tick
(define (tetris-on-tick t ms)
  (~> t
      (tetris-fps-tick ms)
      (tetris-gravity-tick ms)
      (tetris-autoshift-tick ms)))



