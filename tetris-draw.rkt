#lang racket/base

(require 2htdp/image)
(require lang/posn)
(require "utils.rkt")
(require "tetris.rkt")

(provide draw-blocks)


(define h-shape-color
  (make-immutable-hash '((ghost . gray)
                         (L . orange)
                         (J . "Royal Blue")
                         (Z . red)
                         (S . green)
                         (T . "Light Purple")
                         (O . yellow)
                         (I . "Medium Cyan"))))

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
  (let* ([col (hash-ref h-shape-color (block-color b))]
         [bposn (block-posn b)]
         [x (posn-x bposn)]
         [y (posn-y bposn)])
    (underlay/xy scene
                 (* x BLOCK-SIZE)
                 (- PLAYFIELD-HEIGHT
                    (* (+ y 1) BLOCK-SIZE))
                 (block-img col))))


; List of Blocks -> Image
; Draw the list of Blocks on the Playfield
(define (draw-blocks blocks)
  (foldl draw-block EMPTY-PLAYFIELD
         blocks))
