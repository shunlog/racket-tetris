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
  [new-frozen-tetris (-> frozen-tetris?)]))


; -------------------------------
; Requires

(require racket/set)
(require "block.rkt")

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


(define (new-frozen-tetris)
  (frozen-tetris))
