#lang racket/base

(require 2htdp/image)
(require lang/posn)

(require "tetris-logic.rkt")

(provide draw-tetris)


(define BLOCK-SIZE 20) ; blocks are squares
(define (block-img color)
  (overlay
   (square (- BLOCK-SIZE 2) "solid" color)
   (square BLOCK-SIZE "solid" "black")))


(define PLAYFIELD-WIDTH (* WIDTH BLOCK-SIZE))
(define PLAYFIELD-HEIGHT (* HEIGHT BLOCK-SIZE))

; image of empty Playfield
(define EMPTY-PLAYFIELD (empty-scene PLAYFIELD-WIDTH PLAYFIELD-HEIGHT))


;; Block, Image -> Image
;; Draw a Block onto the playfield image
(define (draw-block b scene)
  (underlay/xy scene
               (* (posn-x (block-posn b)) BLOCK-SIZE)
               (- PLAYFIELD-HEIGHT
                  (* (+ (posn-y (block-posn b)) 1) BLOCK-SIZE))
               (block-img (block-color b))))


; List of Blocks -> Image
; Draw the list of Blocks on the Playfield
(define (draw-blocks blocks)
  (foldl draw-block EMPTY-PLAYFIELD
         blocks))


; ; Or(Tetris / Piece / Playfield) -> Image
; Universal function that draws the given elements on the Playfield
(define (draw-tetris t)
  (draw-blocks (tetris-blocks t)))
