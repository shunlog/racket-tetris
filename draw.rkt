#lang racket/base

(require threading)
(require racket/draw)
(require racket/class)
(require "playfield.rkt")
(require "block.rkt")
(require "shapes.rkt")


(define BLOCK-W 20)
(define VANISH-ZONE-H 2)


(provide
 playfield-canvas-size
 draw-playfield
 )


;; Playfield DC -> #:void
(define (draw-playfield plf dc)
  (define rows (playfield-rows plf))
  (define cols (playfield-cols plf))
  (define (draw-block b)
    (define x (* BLOCK-W (block-x b)))
    (define y (* BLOCK-W
                 (- (+ VANISH-ZONE-H (sub1 rows))
                    (block-y b))))
    (define color (hash-ref SHAPE-COLOR (block-type b)))
    (send dc set-brush color 'solid)
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





