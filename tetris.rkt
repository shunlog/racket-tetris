#lang racket/base


;; -------------------------------
;; Constants

(define MS/DROP 1000)               ; ms/cell dropped due to gravity
(define MS/AUTOSHIFT 40)            ; ms/cell moved during autoshift
(define AUTOSHIFT-DELAY 180)        ; ms before autoshift starts
(define MS/SOFT-DROP 50)           ; ms/cell dropped during soft-drop


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
;;   - t-drop: last time the piece was dropped due to gravity
;;   - t-dirn: last time when the piece started moving
;;             (when both direction had been depressed, and one of them was pressed)
(struct tetris [tn pressed-hash t-drop t-dirn t-autoshift t-ticks])


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(define (new-tetris ms
                    #:rows [rows 20]
                    #:cols [cols 10]
                    #:tetrion [tetrion (new-tetrion #:rows rows #:cols cols)])
  (~> (tetris tetrion (hash) ms 0 0 '(0 0 0 0 0))
      (tetris-spawn ms)))


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
  (define other-dirn (if (equal? dirn 'right) 'left 'right))
  (define new-t-dirn (if (tetris--pressed? t other-dirn)
                         (tetris-t-dirn t)
                         ms))
  (~> t
      (tetris--move ms dirn)
      (tetris--set-pressed dirn ms #t)
      (struct-copy tetris _
                   [t-dirn new-t-dirn])))

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
              ([i (in-range n)])
      (tetris--drop t-acc)))
  (~> t
      (drop-n-times times-to-drop)
      (struct-copy tetris _
                   [t-drop new-t-drop])))


(module+ test
  (define t0 (new-tetris 0 #:tetrion (~> (new-tetrion #:rows 2)
                                         (tetrion-spawn 'L))))

  ;; Assert that we got the expected shape
  (check block-lists=?
         (~> t0 tetris-tn tetrion-playfield playfield-blocks)
         (strings->blocks '(".....L...."
                            "...LLL...."
                            ".........."
                            "..........")))

  (test-case
      "Gravity drop one row"
    (define t1 (tetris-gravity-tick t0 (+ 2 MS/DROP)))
    (check block-lists=?
           (~> t1 tetris-tn tetrion-playfield playfield-blocks)
           (strings->blocks '(".........."
                              ".....L...."
                              "...LLL...."
                              "..........")))
    (check-equal? (tetris-t-drop t1) MS/DROP))

  (test-case
      "Gravity drop two rows in a tick"
    (define t2 (tetris-gravity-tick t0
                                    (+ 1 (inexact->exact (* 2 MS/DROP)))))
    (check
     block-lists=?
     (~> t2 tetris-tn tetrion-playfield playfield-blocks)
     (strings->blocks '(".........."
                        ".........."
                        ".....L...."
                        "...LLL....")))
    (check-equal? (tetris-t-drop t2) (* 2 MS/DROP)))

  (test-case
      "Don't gravity drop when not enough time has passed"
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


(define (tetris-spawn t ms [shape-name #f])
  (define tn (tetris-tn t))
  (define new-tn (if shape-name
                     (tetrion-spawn tn shape-name)
                     (tetrion-spawn tn)))
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


;; Move in dirn if enough time has passed since previous move
(define (tetris--autoshift t ms dirn)
  (define t-dirn (tetris-t-dirn t))
  (define t-autoshift
    (if (> t-dirn (tetris-t-autoshift t))
        ;; first autoshift since started holding key, starts at this time
        (+ t-dirn AUTOSHIFT-DELAY)
        (tetris-t-autoshift t)))
  
  (define times-to-move (quotient (- ms t-autoshift) MS/AUTOSHIFT))
  (define new-t-autoshift (+ t-autoshift (* times-to-move MS/AUTOSHIFT)))
  (define t-moved
    (for/fold ([t-acc t])
              ([i (in-range times-to-move)])
      (tetris--move t-acc ms dirn)))
  (struct-copy tetris t-moved
               [t-autoshift new-t-autoshift]))


;; Called each tick,
;; Start autoshifting if it's time to
(define (tetris-autoshift-tick t ms)
  (define right (tetris--pressed? t 'right))
  (define left (tetris--pressed? t 'left))
  (define autoshift? (and (or right left)
                          (> (- ms (tetris-t-dirn t)) AUTOSHIFT-DELAY)))
  (define right-last (> (tetris--pressed-t t 'right) (tetris--pressed-t t 'left)))
  
  (cond
    [(not autoshift?) t]
    [(or (not left) (and right right-last)) (tetris--autoshift t ms 'right)]
    [else (tetris--autoshift t ms 'left)]))


(define (tetris-fps-tick t ms)
  (define ticks (tetris-t-ticks t))
  (define new-ticks (append (cdr ticks) (list ms)))
  (struct-copy tetris t
               [t-ticks new-ticks]))

(define (tetris-fps t)
  (define ticks (tetris-t-ticks t))
  (define dts (for/sum ([t1 ticks]
                        [t2 (cdr ticks)])
                (- t2 t1)))
  (define dt-avg (/ (exact->inexact dts) (sub1 (length ticks))))
  (/ 1000.0 dt-avg))

;; Used in big-bang on every tick.
(define (tetris-on-tick t ms)
  (~> t
      (tetris-fps-tick ms)
      (tetris-gravity-tick ms)
      (tetris-autoshift-tick ms)))



