#lang racket/base

(require racket/contract)


(provide
 (contract-out
  [tetris? (-> any/c boolean?)]
  [new-tetris (-> tetris?)]
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


;; ----------------------------
;; Definitions


;; A Tetris is a struct:
;;   - ft: FrozenTetris
(struct tetris [ft])


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(define (new-tetris)
  (tetris (new-frozen-tetris)))


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
  (define t0 (new-tetris)))



;;;;;;;;;;;;;;
;; Big Bang ;;
;;;;;;;;;;;;;;


(define (tetris-on-tick t ms)
  (~> t
      (tetris-gravity-tick ms)))



