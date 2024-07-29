#lang racket/base

(require racket/contract)


(provide
 (contract-out
  [tetris? (-> any/c boolean?)]
  [new-tetris (->* (natural-number/c)
                   (#:rows natural-number/c
                    #:cols natural-number/c
                    #:shape-generator shape-generator?)
                   tetris?)]
  [tetris-ft (-> tetris? frozen-tetris?)]
  [tetris-pressed-left (-> tetris? natural-number/c tetris?)]
  [tetris-pressed-right (-> tetris? natural-number/c tetris?)]
  [tetris-hard-drop (-> tetris? natural-number/c tetris?)]
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

(define MS/DROP (inexact->exact (floor 1000))) ; ms before we drop due to gravity


;; ----------------------------
;; Definitions


;; A Tetris is a struct:
;;   - ft: FrozenTetris
(struct tetris [ft shape-generator t-drop])


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(define (new-tetris ms
                    #:cols [cols 10]
                    #:rows [rows 20]
                    #:shape-generator [shape-generator 7-loop-shape-generator])
  (define ft (new-frozen-tetris #:cols cols
                                #:rows rows))
  (~> (tetris ft shape-generator ms)
      (tetris-spawn ms)))


(define (tetris-pressed-left t ms)
  (define ft (tetris-ft t))
  (define new-ft
    (with-handlers ([exn:fail? (λ (e) ft)])
      (frozen-tetris-left ft)))
  (struct-copy tetris t
               [ft new-ft]) )


(define (tetris-pressed-right t ms)
  (define ft (tetris-ft t))
  (define new-ft
    (with-handlers ([exn:fail? (λ (e) ft)])
      (frozen-tetris-right ft)))
  (struct-copy tetris t
               [ft new-ft]))


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


;; Apply gravity or soft-drop if "down" is being held.
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
  (define t0 (new-tetris
              0
              ;; Guarantee that the first shape will be 'L
              #:shape-generator 7-loop-shape-generator
              #:rows 2))

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


(define (tetris-spawn t ms                             
                      #:piece [piece ((tetris-shape-generator t))])
  (define new-ft (~> (tetris-ft t)
                     (frozen-tetris-spawn piece)))
  ;; drop immediately if possible, so the player can see the block
  (define ft-dropped (with-handlers ([exn:fail? (λ (e) new-ft)])
                       (frozen-tetris-drop new-ft)))
  (struct-copy tetris t [ft ft-dropped] [t-drop ms]))


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


;; Used in big-bang on every tick.
(define (tetris-on-tick t ms)
  (~> t
      (tetris-gravity-tick ms)))



