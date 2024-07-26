#lang racket/base

(require racket/contract)


(provide
 (contract-out
  [tetris? (-> any/c boolean?)]
  [new-tetris (-> tetris?)]
  [tetris-ft (-> tetris? frozen-tetris?)]
  [tetris-pressed-left (-> tetris? natural-number/c tetris?)]
  [tetris-pressed-right (-> tetris? natural-number/c tetris?)]
  [tetris-drop (-> tetris? natural-number/c tetris?)]))

; -------------------------------
; Requires


(require threading)
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


(define (tetris-drop t ms)
  (define ft (tetris-ft t))
  (define new-ft
    (with-handlers ([exn:fail? (λ (e) ft)])
      (frozen-tetris-drop ft)))
  (struct-copy tetris t
               [ft new-ft]))
