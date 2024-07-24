#lang racket/base

;; This module implements a "Frozen" Tetris.
;; Think of it as a Tetris that's been frozen in time:
;; all the inputs that have an immediate effect still work,
;; whereas everything that happens with a delay doesn't.
;;
;; So pretty much everything is handled:
;;   - pressing Left/Right moves the piece
;;   - pressing Space drops it
;;   - you can move the piece down with a function call
;;   - you can lock the piece with a function call
;;   - piece spawn, score update, game over check
;;
;; Except for actions that need information about time:
;;   - autoshift
;;   - gravity
;;   - lock delay


(require racket/contract)
(provide
 (contract-out
  [new-frozen-tetris (-> frozen-tetris?)]
  [frozen-tetris-blocks (-> frozen-tetris? (listof block?))]))


; -------------------------------
; Requires


(require racket/set)
(require lang/posn)
(require "block.rkt")
(require "playfield.rkt")
(require "utils.rkt")


;; ----------------------------
;; Definitions


; A FrozenTetris is a struct:
; - piece: Piece
; - locked: Playfield
(struct frozen-tetris [piece locked])


; A Piece is a struct:
; - posn: Posn
; - shape-name: one of shape-names
(struct piece [posn shape-name rotation])

; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(define (new-frozen-tetris)
  (frozen-tetris (piece (make-posn 4 18) 'L 0) (empty-playfield)))


(module+ test
  (test-case
      "Create a frozen tetris"
    ;; A piece must be spawned during init
    (define ft0 (new-frozen-tetris))
    (check-true
     (piece? (frozen-tetris-piece ft0)))
    ))


; FrozenTetris -> (Listof Block)
(define (frozen-tetris-blocks ft)
  '())

(module+ test
  (test-case
      "Get all blocks from a FrozenTetris"

    ;; Just a piece
    (define ft0
      (frozen-tetris
       (piece (make-posn 0 0) 'T 0)
       (empty-playfield)))
    (check
     block-lists=?
     (frozen-tetris-blocks ft0)
     (strings->blocks '("..."
                          ".T."
                          "TTT")))

    ))
