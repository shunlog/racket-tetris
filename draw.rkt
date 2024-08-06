#lang racket/base

(require threading)
(require racket/draw)
(require racket/class)
(require lang/posn)
(require "playfield.rkt")
(require "block.rkt")
(require "shapes.rkt")
(require "draw-utils.rkt")

(define BLOCK-W 20)
(define VANISH-ZONE-H 2)
(define GARBAGE-COLOR (make-color 156 154 154))
(define GHOST-ALPHA 0.3)

(provide
 playfield-canvas-size
 draw-playfield
 )


;; Block -> Color
(define (block-color b)
  (define bt (block-type b))
  (cond
    [(equal? 'garbage bt) GARBAGE-COLOR]
    [else
     (define shape-color (hash-ref SHAPE-COLOR (car bt)))
     (cond [(equal? 'normal (cdr bt)) shape-color]
           [(equal? 'ghost (cdr bt)) (set-alpha shape-color GHOST-ALPHA)])]))


;; Playfield DC -> #:void
(define (draw-playfield plf dc)
  (define rows (playfield-rows plf))
  (define cols (playfield-cols plf))
  (define (draw-block b)
    (define x (* BLOCK-W (posn-x (block-posn b))))
    (define y (* BLOCK-W
                 (- (+ VANISH-ZONE-H (sub1 rows))
                    (posn-y(block-posn b)))))
    (send dc set-brush (block-color b) 'solid)
    (send dc draw-rectangle x y BLOCK-W BLOCK-W))

  (send dc set-brush "gray" 'solid)  
  (send dc draw-rectangle 0 0 (* BLOCK-W cols) (* BLOCK-W VANISH-ZONE-H))
  (for ([block (playfield-blocks plf)])
    (draw-block block)))



; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(define (playfield-canvas-size plf)
  (define w (* BLOCK-W (playfield-cols plf)))
  (define h (* BLOCK-W (+ (playfield-rows plf)
                          VANISH-ZONE-H)))
  (values w h))


(module+ test
  (displayln "Drawing two sets of tetrominoes in a 4x10")
  (define plf0
    (~> (empty-playfield 10 5)
        (playfield-add-blocks
         (strings->blocks '("..."
                            "LLLZZIIIII"
                            "LTTTZZOOJI"
                            "OOTSSTOOJI"
                            "OOSSTTTJJI")))))


  (define-values (w h) (playfield-canvas-size plf0))
  (define dc (new bitmap-dc% [bitmap (make-bitmap w h)]))
  (draw-playfield plf0 dc)
  (send dc get-bitmap))





