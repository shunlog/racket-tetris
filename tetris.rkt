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
  [tetris-on-tick (-> tetris? natural-number/c tetris?)]
  ;; [tetris-on-key (-> tetris? )]
  ))

; -------------------------------
; Requires


(require threading)
(require 2htdp/universe)
(require "frozen-tetris.rkt")
(require "playfield.rkt")
(require "block.rkt")
(require "utils.rkt")
(require "shapes.rkt")


;; -------------------------------
;; Constants

(define GRAVITY 1)                      ; cells/second
(define SOFT-DROP 60)                   ; cells/second


;; ----------------------------
;; Definitions


;; A Tetris is a struct:
;;   - ft: FrozenTetris
(struct tetris [ft t-drop])


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
  (define ft (new-frozen-tetris (shape-generator)
                                #:cols cols
                                #:rows rows))
  (tetris ft ms))


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


;; Apply gravity or soft-drop if "down" is being held.
(define (tetris-gravity-tick t ms)
  (define ft (tetris-ft t))
  (define new-ft
    (with-handlers ([exn:fail? (λ (e) ft)])
      (frozen-tetris-drop ft)))
  (struct-copy tetris t
               [ft new-ft]))

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

  (define ms/drop (inexact->exact (ceiling (/ 1000 GRAVITY))))
  
  (test-case
      "Gravity drop one row"
    (define t1 (tetris-gravity-tick t0 (+ 2 ms/drop)))
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
     ms/drop))

  (test-case
      "Gravity drop two rows in a tick"
    (define t2 (tetris-gravity-tick t0
                                    (+ 1 (inexact->exact (* 2 ms/drop)))))
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
     (* 2 ms/drop)))

  (test-case
      "Don't gravity drop when not enough time has passed"
    (define t3 (tetris-gravity-tick t0 (inexact->exact (* 0.5 ms/drop))))
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



;; Used in big-bang on every tick.
(define (tetris-on-tick t ms)
  (~> t
      (tetris-gravity-tick ms)))



