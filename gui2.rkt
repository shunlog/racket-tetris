#lang racket/base
(require threading
         racket/match
         racket/class
         racket/draw
         racket/gui/base
         mode-lambda
         mode-lambda/backend/gl
         mode-lambda
         mode-lambda/static
         racket/fixnum
         pict
         pict/flash
         "draw.rkt"
         "block.rkt"
         lang/posn
         "shapes.rkt"
         "playfield.rkt")


(define LAYERS 8)                       ; safe limit is 256
(define COLS 10)
(define ROWS 20)
(define VANISH-LINES 2)
(define ROWS-TOTAL (+ ROWS VANISH-LINES))
(define W (* BLOCK-W COLS))
(define H (* BLOCK-W ROWS-TOTAL))

;;;
;;; SPRITE database
;;;

(define db (make-sprite-db))

(define (tile->sym tile)
  (string->symbol (string-append "normal-" (symbol->string (tile-shape tile)))))

(for ([sn SHAPE-NAMES])
  (define tile (tile-normal sn))
  (define tilepict (tile-pict tile))
  (add-sprite!/value db (tile->sym tile) tilepict))

(define cdb (compile-sprite-db db))


;;;
;;; Mode-Lambda set-up
;;; 


;;; it should only be initialized once
(define ml-draw
  (stage-draw/dc cdb W H LAYERS))

;;; These lists will be mutated
(define static (list ))
(define dynamic (list ))

;;; This vector can be mutated as well, but that's advanced
(define ml-layers
  (make-vector LAYERS (layer (* W 0.5) (* H 0.5))))


;;; 
;;; GUI
;;;
;;; Interface:
;;; set! the following vars:
;;; 1. static - list of sprites (optimized)
;;; 2. dynamic - list of sprites
;;; 3. ml-layers - vector of (layer) of size LAYERS


(define gl-conf (new gl-config%))
(send gl-conf set-hires-mode #t)
(send gl-conf set-legacy? #f)


(define f
  (new frame% [label "Quick"] [width W] [height H]))
(define c
  (new canvas%
       [parent f]
       [min-width W]
       [min-height H]
       [gl-config gl-conf]
       [style '(no-autoclear gl)]
       [paint-callback
        (λ (c dc)
          (define draw (ml-draw ml-layers static dynamic))
          (match/values (send c get-gl-client-size)
            [(w h) (draw w h dc)]))]))

(send f show #t)



;;;
;;; RUNTIME
;;;

(define plf0
  (~> (empty-playfield 2 3)
      (playfield-add-blocks
       (strings->blocks '(".S"
                          ".."
                          "II"
                          "J."
                          "LL")))))

(define plf0-sprites
  (for/list ([blck (playfield-blocks plf0)])
   (define col (posn-x (block-posn blck)))
   (define row (posn-y (block-posn blck)))
   (define x col)
   (define y (- ROWS-TOTAL row 1))
   (define cx (fx->fl (+ (* BLOCK-W x) (/ BLOCK-W 2))))
   (define cy (fx->fl (+ (* BLOCK-W y) (/ BLOCK-W 2))))
   (define sprite-id (sprite-idx cdb (tile->sym (block-tile blck))))
   (sprite cx cy sprite-id)))

(set! dynamic plf0-sprites)
