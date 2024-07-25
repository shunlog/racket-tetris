#lang racket/base


; Draws Tetris data structures

(require racket/contract)
(require "playfield.rkt")
(require "block.rkt")
(require "frozen-tetris.rkt")


(provide
 (contract-out
  [draw-playfield (-> playfield? image?)]
  ))


;; --------------------------
;; Configuration constants

(define BLOCK-W 20)

(define BLOCK-COLOR-HASH
  (hash 'L "dark orange"
        'J "medium blue"
        'S 'green
        'Z "red"
        'T "dark violet"
        'I "aqua"
        'O 'yellow
        'ghost "lightgray"
        'garbage "gray"))


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
  
  (define (draw-block-on-grid b grid)
    (define x (* BLOCK-W (block-x b)))
    (define y (* BLOCK-W (- rows (block-y b))))
    (place-image/align (draw-block b)
                       x y
                       "left" "bottom"
                       grid))
  (define (draw-grid)
    (foldr draw-block-on-grid
           (rectangle grid-width grid-height "solid" "white")
           (playfield-blocks p)))
  
  (~> (draw-grid)
      (draw-border-around-img 3 "gray")
      (draw-border-around-img 5 "white")
      (draw-border-around-img 3 "darkgray")))


;; ....
;; .■..
;; ■■■.
(module+ test
  (displayln "Drawing a T on a 3x4 grid:")
  (~> (empty-playfield 4 3)
      (playfield-add-block*
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
      (playfield-add-block* blocks)
      (draw-playfield)))


(define (draw-frozen-tetris ft)
  (draw-playfield (frozen-tetris-playfield ft)))


(module+ test
  (displayln "Drawing a new FrozenTetris")
  (define ft0 (new-frozen-tetris))
  (define ft-drop1 (frozen-tetris-drop ft0))
  (define ft-drop2 (frozen-tetris-drop ft-drop1))
  (define ft-drop3 (frozen-tetris-drop ft-drop2))
  (beside (~> ft0 frozen-tetris-playfield draw-playfield)
          (~> ft-drop1 draw-frozen-tetris)
          (~> ft-drop2 draw-frozen-tetris)
          (~> ft-drop3 draw-frozen-tetris)))
