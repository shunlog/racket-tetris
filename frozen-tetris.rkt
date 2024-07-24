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
  [frozen-tetris-playfield (-> frozen-tetris? playfield?)]))


; -------------------------------
; Requires


(require racket/set)
(require lang/posn)
(require threading)
(require "block.rkt")
(require "playfield.rkt")
(require "shapes.rkt")
(require "utils.rkt")


;; ----------------------------
;; Definitions


; A FrozenTetris is a struct:
; - piece: Piece
; - locked: Playfield
(struct frozen-tetris [piece locked shape-generator])


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


(define (frozen-tetris-spawn ft [shape-name #f])
  (define sname
    (if (not shape-name)
        ((frozen-tetris-shape-generator ft))
        shape-name))
  (define grid-cols
    (~> ft frozen-tetris-locked playfield-cols))
  (define grid-rows
    (~> ft frozen-tetris-locked playfield-rows))
  (define piece-x
    (floor (/ (- grid-cols (shape-width sname)) 2)))
  (define y-spawn
    ; the lowest blocks should spawn on the first line of the vanish zone,
    ; so if there are 20 rows, it should spawn on row with index 20 (0-based)
    grid-rows)
  (define piece-y
    (- y-spawn (shape-gap-below sname)))
  (define new-piece
    (piece (make-posn piece-x piece-y)
           sname
           0))
  (struct-copy frozen-tetris ft
               [piece new-piece]))


(module+ test
  (test-case
      "Spawn a piece"
    (define ft0
      (frozen-tetris (piece (make-posn 0 0) 'L 0)
                     (empty-playfield 10 2)
                     7-loop-shape-generator))
    (check
     block-lists=?
     (~> ft0
         (frozen-tetris-spawn 'L)
         frozen-tetris-playfield
         playfield-blocks)
     (strings->blocks '(".....L...."
                        "...LLL...."
                        ".........."
                        "..........")))

    (check
     block-lists=?
     (~> ft0
         (frozen-tetris-spawn 'I)
         frozen-tetris-playfield
         playfield-blocks)
     (strings->blocks '("...IIII..."
                        ".........."
                        "..........")))
    ))


(define (new-frozen-tetris)
  (frozen-tetris-spawn
   (frozen-tetris #f (empty-playfield) 7-loop-shape-generator)))


(define (piece-blocks piece)
  (define sname (piece-shape-name piece))
  (define rot (piece-rotation piece))
  (define blocks-at-origin (shape-name->blocks sname rot))
  (for/list ([blck blocks-at-origin])
    (block-move blck (piece-posn piece))))


; FrozenTetris -> Playfield
(define (frozen-tetris-playfield ft)
  (define locked (frozen-tetris-locked ft))
  (define piece (frozen-tetris-piece ft))
  (playfield-add-block*
   locked
   (piece-blocks piece)))


(module+ test
  (test-case
      "Get all blocks from a FrozenTetris"

    ;; A Piece and some locked blocks
    (define plf1
      (playfield-add-block*
       (empty-playfield)
       (strings->blocks '("JJ."))))
    (define ft1
      (frozen-tetris
       (piece (make-posn 0 0) 'T 0)
       plf1
       7-loop-shape-generator))
    (check
     block-lists=?
     (playfield-blocks (frozen-tetris-playfield ft1))
     (playfield-blocks (playfield-add-block*
                        plf1
                        (strings->blocks '(".T."
                                           "TTT"
                                           "...")))))))
