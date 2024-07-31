#lang racket/base


; Draws Tetris data structures

(require racket/contract)
(provide
 (contract-out
  [draw-playfield (-> playfield? image?)]
  [draw-tetrion (-> tetrion? image?)]
  [draw-tetris (-> tetris? image?)]
  ))


;; --------------------------
;; Configuration constants

(define BLOCK-W 20)
(define VANISH-AREA-ROWS 2)

(define BLOCK-TYPE-COLOR
  (hash 'L (make-color 255 128 0)
        'J (make-color 0 132 255)
        'S (make-color 0 217 51)
        'Z (make-color 245 7 7)
        'T (make-color 205 7 245)
        'I (make-color 0 247 255)
        'O (make-color 242 235 12)
        'garbage (color 156 154 154)))


;; --------------------------
;; Imports

(require threading)
(require 2htdp/image)
(require "playfield.rkt")
(require "block.rkt")
(require "shapes.rkt")
(require "tetrion.rkt")
(require "tetris.rkt")
(require "image-utils.rkt")


;; --------------------------
;; Implementation

; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(module+ test
  (for ([type `(,@SHAPE-NAMES 'garbage)])
    (test-case
        "Colors specified for all block types"
      (check-not-exn
       (λ () (hash-ref BLOCK-TYPE-COLOR type))
       (format "No color specified for type ~v in BLOCK-COLOR-HASHs hash." type)))))


;; Pretty blocks with borders,
;; but slower
(define (draw-block2 b)
  (define type-color (hash-ref BLOCK-TYPE-COLOR (block-type b)))
  (define block-color
    (if (not (block-ghost b))
        type-color
        (struct-copy color type-color [alpha 100])))
  (define outline-color (darker block-color))
  (define border-width (inexact->exact (ceiling (/ BLOCK-W 8))))
  ;; the pen draws half of the border outside the square width
  (define square-width (- BLOCK-W border-width))  
  (overlay (square square-width
                   "outline"
                   (make-pen outline-color border-width "solid" "round" "round"))
           (square (- square-width border-width) "solid" block-color)
           ;; bring the image to the correct width
           (square BLOCK-W "solid" (make-color 0 0 0 0))))


;; The fastest drawing method
(define (draw-block b)
  (define type-color (hash-ref BLOCK-TYPE-COLOR (block-type b)))
  (define block-color
    (if (not (block-ghost b))
        type-color
        (struct-copy color type-color [alpha 100])))  
  (square BLOCK-W "solid" block-color))


(module+ test
  ;; Draw a table with all the block types, also in ghost form
  (for*/fold ([rows empty-image])
             ([type `(garbage ,@SHAPE-NAMES)]
              [ghost '(#f #t)])
    (above rows
           (beside (overlay/align "left" "middle"
                                  (text (format "~v ~a" type (if ghost "ghost" "")) 16 "black")
                                  (rectangle 120 (+ 10 BLOCK-W) "solid" "white"))
                   (draw-block (block 0 0 type ghost))))))


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
  (displayln "Drawing a ghost T on a 3x4 grid:")
  (~> (empty-playfield 4 3)
      (playfield-add-blocks
       (list
        (block 0 0 'T #t)
        (block 1 0 'T #t)
        (block 2 0 'T #t)
        (block 1 1 'T #t)))
      draw-playfield))



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


(define (draw-tetrion tn)
  (draw-playfield (tetrion-playfield tn)))


(module+ test
  (displayln "Drawing a new Tetrion")
  (define ft0 (new-tetrion #:starting-shape 'L))
  (define tn-drop1 (tetrion-drop ft0))
  (define tn-drop2 (tetrion-drop tn-drop1))
  (define tn-drop3 (tetrion-drop tn-drop2))
  (beside (~> ft0 tetrion-playfield draw-playfield)
          (~> tn-drop1 draw-tetrion)
          (~> tn-drop2 draw-tetrion)
          (~> tn-drop3 draw-tetrion)))

(define (draw-tetris t)
  (draw-tetrion (tetris-tn t)))
