#lang racket/base


; Draws Tetris data structures

(require racket/contract)
(require "playfield.rkt")
(require "block.rkt")
(require "frozen-tetris.rkt")
(require "tetris.rkt")

(provide
 (contract-out
  [draw-playfield (-> playfield? image?)]
  [draw-frozen-tetris (-> frozen-tetris? image?)]
  [draw-tetris (-> tetris? image?)]
  ))


;; --------------------------
;; Configuration constants

(define BLOCK-W 15)
(define VANISH-AREA-ROWS 2)

(define BLOCK-COLOR-HASH
  (hash 'L (make-color 255 128 0)
        'J (make-color 0 132 255)
        'S (make-color 0 217 51)
        'Z (make-color 245 7 7)
        'T (make-color 205 7 245)
        'I (make-color 0 247 255)
        'O (make-color 242 235 12)
        'ghost (color 196 196 196)
        'garbage (color 156 154 154)))


;; --------------------------
;; Imports

(require threading)
(require 2htdp/image)


;; --------------------------
;; Implementation

; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(module+ test
  (for ([t BLOCK-TYPES])
    (define color (hash-ref BLOCK-COLOR-HASH t #f))
    (check-not-false color (format "No color specified for type ~v in BLOCK-COLOR-HASHs hash." t))
    (check-true
     (image-color? color)   
     (format "Not a color: ~v (for type ~v in BLOCK-COLOR-HASHs hash)." color t))))


(define (draw-block b)
  (define type (block-type b))
  (define color (hash-ref BLOCK-COLOR-HASH type))
  (overlay
   (square (- BLOCK-W 2) "solid" color)
   (square BLOCK-W
           "solid"
           (make-color 30 30 30))))


; Image Natural Color -> Image
(define (draw-border-around-img img thickness color)
  (define img-width (image-width img))
  (define img-height (image-height img))
  (define width (+ thickness img-width))
  (define height (+ thickness img-height))
  (overlay/align "middle" "middle"
                 img
                 (rectangle width height "solid" color)))


(define (draw-playfield p)
  (define cols (playfield-cols p))
  (define rows (playfield-rows p))
  (define grid-width (* BLOCK-W cols))
  (define grid-height (* BLOCK-W rows))
  (define vanish-area-height (* BLOCK-W VANISH-AREA-ROWS))
  (define grid (above
                (rectangle grid-width vanish-area-height  "solid" "lightgray")
                (rectangle grid-width grid-height "solid" "white")))
  
  (define (draw-block-on-grid b grid)
    (define x (* BLOCK-W (block-x b)))
    (define y (* BLOCK-W (+ (- rows (block-y b))
                            VANISH-AREA-ROWS)))
    (place-image/align (draw-block b)
                       x y
                       "left" "bottom"
                       grid))
  (define (draw-blocks-on-grid)
    (foldr draw-block-on-grid
           grid
           (playfield-blocks p)))
  
  (~> (draw-blocks-on-grid)
      (draw-border-around-img 3 "gray")
      (draw-border-around-img 5 "white")
      (draw-border-around-img 3 "darkgray")))


;; ....
;; .■..
;; ■■■.
(module+ test
  (displayln "Drawing a T on a 3x4 grid:")
  (~> (empty-playfield 4 3)
      (playfield-add-blocks
       (list
        (block 0 0 'T)
        (block 1 0 'T)
        (block 2 0 'T)
        (block 1 1 'T)))
      (draw-playfield)))



(module+ test
  (displayln "Drawing two sets of tetrominoes in a 4x10")
  (define blocks (strings->blocks
                  '("..."
                    "LLLZZIIIII"
                    "LTTTZZOOJI"
                    "OOTSSTOOJI"
                    "OOSSTTTJJI")))
  (~> (empty-playfield 10 5)
      (playfield-add-blocks blocks)
      (draw-playfield)))


(define (draw-frozen-tetris ft)
  (draw-playfield (frozen-tetris-playfield ft)))


(module+ test
  (displayln "Drawing a new FrozenTetris")
  (define ft0 (new-frozen-tetris #:starting-shape 'L))
  (define ft-drop1 (frozen-tetris-drop ft0))
  (define ft-drop2 (frozen-tetris-drop ft-drop1))
  (define ft-drop3 (frozen-tetris-drop ft-drop2))
  (beside (~> ft0 frozen-tetris-playfield draw-playfield)
          (~> ft-drop1 draw-frozen-tetris)
          (~> ft-drop2 draw-frozen-tetris)
          (~> ft-drop3 draw-frozen-tetris)))

(define (draw-tetris t)
  (draw-frozen-tetris (tetris-ft t)))
