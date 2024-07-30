#lang racket/base

(require racket/contract)


(provide
 (contract-out
  [tetris? (-> any/c boolean?)]
  [new-tetris (->* (natural-number/c)
                   (#:frozen-tetris frozen-tetris?)
                   tetris?)]
  [tetris-ft (-> tetris? frozen-tetris?)]

  ;; Movement
  [tetris-left-pressed (-> tetris? natural-number/c tetris?)]
  [tetris-left-released (-> tetris? natural-number/c tetris?)]
  [tetris-right-pressed (-> tetris? natural-number/c tetris?)]
  [tetris-right-released (-> tetris? natural-number/c tetris?)]
  [tetris-rotate-cw (-> tetris? natural-number/c tetris?)]
  [tetris-rotate-ccw (-> tetris? natural-number/c tetris?)]  
  [tetris-hard-drop (-> tetris? natural-number/c tetris?)]

  ;; Others
  [tetris-on-tick (-> tetris? natural-number/c tetris?)]
  ))

; -------------------------------
; Requires


(require threading)
(require "frozen-tetris.rkt")
(require "playfield.rkt")
(require "block.rkt")
(require "utils.rkt")
(require "shapes.rkt")


;; -------------------------------
;; Constants

(define MS/DROP 1000)               ; ms before we drop due to gravity
(define AUTOSHIFT 200)              ; ms before autoshift starts


;; ----------------------------
;; Definitions


;; A Tetris is a struct:
;;   - ft: FrozenTetris
;;   - t-drop: last time the piece was dropped due to gravity
;;   - t-dirn: last time when the piece started moving
;;             (when both direction had been depressed, and one of them was pressed)
(struct tetris [ft pressed-hash t-drop t-dirn])


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(define (new-tetris ms
                    #:frozen-tetris [frozen-tetris (new-frozen-tetris)])
  (~> (tetris frozen-tetris (hash) ms 0)
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


;; call frozen-tetris-move and update the lock timer if successful move
(define (tetris--move t ms dirn)
  (define ft (tetris-ft t))
  (define new-ft
    (with-handlers ([exn:fail? (λ (e) ft)])
      (if (equal? dirn 'right)
          (frozen-tetris-right ft)
          (frozen-tetris-left ft))))
  (struct-copy tetris t [ft new-ft]))


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

(define (tetris-right-released t ms)
  (tetris--set-pressed t 'right ms #f))

(define (tetris-left-released t ms)
  (tetris--set-pressed t 'left ms #f))


;; Tetris -> Tetris
;; simply drop the piece,
;; and update the t-on-ground
(define (tetris--drop t)
  (define ft (tetris-ft t))
  (define new-ft
    (with-handlers ([exn:fail? (λ (e) ft)])
      (frozen-tetris-drop ft)))
  (struct-copy tetris t
               [ft new-ft]))


;; Apply gravity or soft-drop if it's time to and "down" is being held.
(define (tetris-gravity-tick t ms)
  (define t-drop (tetris-t-drop t))
  (define times-to-drop (quotient (- ms t-drop) MS/DROP))
  (define new-t-drop (+ t-drop (* times-to-drop MS/DROP)))
  (define (drop-n-times t n)
    (for/fold ([t-acc t])
              ([i (in-range n)])
      (tetris--drop t-acc)))
  (~> t
      (drop-n-times times-to-drop)
      (struct-copy tetris _
                   [t-drop new-t-drop])))


(module+ test
  (define t0
    (new-tetris
     0
     #:frozen-tetris (new-frozen-tetris
                      #:starting-shape 'L
                      #:rows 2)))

  ;; Assert that we got the expected shape
  (check                       
   block-lists=?
   (~> t0
       tetris-ft
       frozen-tetris-playfield
       playfield-blocks)
   (strings->blocks '(".....L...."
                      "...LLL...."
                      ".........."
                      "..........")))

  
  (test-case
      "Gravity drop one row"
    (define t1 (tetris-gravity-tick t0 (+ 2 MS/DROP)))
    (check
     block-lists=?
     (~> t1
         tetris-ft
         frozen-tetris-playfield
         playfield-blocks)
     (strings->blocks '(".........."
                        ".....L...."
                        "...LLL...."
                        "..........")))
    (check-equal?
     (tetris-t-drop t1)
     MS/DROP))

  (test-case
      "Gravity drop two rows in a tick"
    (define t2 (tetris-gravity-tick t0
                                    (+ 1 (inexact->exact (* 2 MS/DROP)))))
    (check
     block-lists=?
     (~> t2
         tetris-ft
         frozen-tetris-playfield
         playfield-blocks)
     (strings->blocks '(".........."
                        ".........."
                        ".....L...."
                        "...LLL....")))
    (check-equal?
     (tetris-t-drop t2)
     (* 2 MS/DROP)))

  (test-case
      "Don't gravity drop when not enough time has passed"
    (define t3 (tetris-gravity-tick t0 (inexact->exact (* 0.5 MS/DROP))))
    (check
     block-lists=?
     (~> t3
         tetris-ft
         frozen-tetris-playfield
         playfield-blocks)
     (strings->blocks '(".....L...."
                        "...LLL...."
                        ".........."
                        "..........")))
    (check-equal?
     (tetris-t-drop t3)
     0))
  )


(define (tetris-spawn t ms [shape-name #f])
  (define ft (tetris-ft t))
  (define new-ft (if shape-name
                     (frozen-tetris-spawn ft shape-name)
                     (frozen-tetris-spawn ft)))
  (struct-copy tetris t [ft new-ft] [t-drop ms]))


(define (tetris-lock t ms)
  (define ft-locked (~> (tetris-ft t)
                        frozen-tetris-lock))
  (~> (struct-copy tetris t [ft ft-locked])
      (tetris-spawn ms)))


(define (tetris-hard-drop t ms)
  (define ft-dropped (~> (tetris-ft t)
                         frozen-tetris-hard-drop))
  (~> (struct-copy tetris t [ft ft-dropped])
      (tetris-lock ms)))


(define (tetris--rotate t cw? ms)
  (define new-ft (frozen-tetris-rotate (tetris-ft t) cw?))
  (struct-copy tetris t
               [ft new-ft]))


(define (tetris-rotate-cw t ms)
  (tetris--rotate t #t ms))

(define (tetris-rotate-ccw t ms)
  (tetris--rotate t #f ms))


;; Apply autoshift if it's time to and a direction key is being held
(define (tetris-autoshift-tick t ms)
  (define right (tetris--pressed? t 'right))
  (define left (tetris--pressed? t 'left))
  (define autoshift? (and (or right left)
                          (> (- ms (tetris-t-dirn t)) AUTOSHIFT)))
  (define right-last (> (tetris--pressed-t t 'right) (tetris--pressed-t t 'left)))
  
  (cond
    [(not autoshift?) t]
    [(or (not left) (and right right-last)) (tetris--move t ms 'right)]
    [else (tetris--move t ms 'left)]))


;; Used in big-bang on every tick.
(define (tetris-on-tick t ms)
  (~> t
      (tetris-gravity-tick ms)
      (tetris-autoshift-tick ms)))



