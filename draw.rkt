#lang racket/base


; Draws Tetris data structures

(require racket/contract)
(require "playfield.rkt")
(require "block.rkt")

(provide
 (contract-out
  [draw-playfield (-> playfield? image?)]
  ))


;; --------------------------
;; Configuration constants

(define BLOCK-W 20)

(define block-color
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
  (for ([t block-types])
    (define color (hash-ref block-color t #f))
    (check-not-false color (format "No color specified for type ~v in block-colors hash." t))
    (check-true
     (image-color? color)   
     (format "Not a color: ~v (for type ~v in block-colors hash)." color t))))


(define (block-image b)
  (define type (block-type b))
  (define color (hash-ref block-color type))
  (overlay
   (square (- BLOCK-W 1) "solid" color)
   (square BLOCK-W "solid" (make-color 20 20 20))))



(define (draw-playfield p)
  (define cols (playfield-width p))
  (define rows (playfield-height p))

  (define (draw-block b grid)
    (define x (* BLOCK-W (block-x b)))
    (define y (* BLOCK-W (- rows (block-y b))))
    (place-image/align (block-image b)
                       x y
                       "left" "bottom"
                       grid))
  (foldr draw-block
         (empty-scene (* BLOCK-W cols)
                      (* BLOCK-W rows))
         (playfield-blocks p)))


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
  (define blocks (strings-to-blocks
                  '("..."
                    "LLLZZIIIII"
                    "LTTTZZOOJI"
                    "OOTSSTOOJI"
                    "OOSSTTTJJI")))
  (~> (empty-playfield 10 5)
      (playfield-add-block* blocks)
      (draw-playfield)))
