#lang racket/base

(require threading)
(require racket/draw)
(require racket/class)
(require lang/posn)
(require pict)
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
 queue-pict
 hold-piece-pict
 )


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


;; Block -> Color
(define (block-color b)
  (define bt (block-type b))
  (cond
    [(equal? 'garbage bt) GARBAGE-COLOR]
    [else
     (define shape-color (hash-ref SHAPE-COLOR (car bt)))
     (cond [(equal? 'normal (cdr bt)) shape-color]
           [(equal? 'ghost (cdr bt)) (set-alpha shape-color GHOST-ALPHA)])]))


(define (draw-block b dc x y)
  (define old-brush (send dc get-brush))
  (send dc set-brush (block-color b) 'solid)
  (send dc draw-rectangle x y BLOCK-W BLOCK-W)
  (send dc set-brush old-brush))


;; Block -> Pict
(define (block-pict b)
  (dc (λ (dc dx dy)
        (draw-block b dc dx dy))
      BLOCK-W BLOCK-W))


;; Playfield DC -> #:void
(define (draw-playfield plf dc)
  (define rows (playfield-rows plf))
  (define cols (playfield-cols plf))
  (send dc set-brush "gray" 'solid)
  (send dc draw-rectangle 0 0 (* BLOCK-W cols) (* BLOCK-W VANISH-ZONE-H))
  (for ([block (playfield-blocks plf)])
    (define row (posn-y (block-posn block)))
    (define col (posn-x (block-posn block)))
    (define x (* BLOCK-W col))
    (define y (* BLOCK-W (- (+ VANISH-ZONE-H (sub1 rows)) row)))
    (draw-block block dc x y)))

;; Get the size of the image for the previous function
(define (playfield-canvas-size plf)
  (define w (* BLOCK-W (playfield-cols plf)))
  (define h (* BLOCK-W (+ (playfield-rows plf)
                          VANISH-ZONE-H)))
  (values w h))


(module+ test
  (test-case
      "Drawing a Playfield with two sets of tetrominoes"
    (displayln "Drawing a Playfield with two sets of tetrominoes")
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
    (send dc get-bitmap)))


;; shape-name/c -> pict
(define (shape-pict sn)
  (define posns (shape-name->posns sn 0))
  (define max-row (apply max (map posn-y posns)))
  (define max-col (apply max (map posn-x posns)))
  (define min-row (apply min (map posn-y posns)))
  (define min-col (apply min (map posn-x posns)))
  (define W (* BLOCK-W (add1 (- max-col min-col))))
  (define H (* BLOCK-W (add1 (- max-row min-row))))
  (for/fold ([img (blank W H)])
            ([pos posns])
    (define row (posn-y pos))
    (define col (posn-x pos))
    (define dy (* BLOCK-W (- max-row row)))
    (define dx (* BLOCK-W (- col min-col)))
    (define blck (block (make-posn 0 0) (cons sn 'normal)))
    (pin-over img dx dy (block-pict blck)))
  )


(module+ test
  (test-case
      "Drawing a shape"
    (displayln "Drawing a shape")
    (shape-pict 'L)))


;; (or/c #f shape-name) -> pict
(define (hold-piece-pict sn)
  (define bg (blank (* 5 BLOCK-W) (* 5 BLOCK-W)))
  (cond
    [(not sn) bg]
    [else (cc-superimpose bg (shape-pict sn))]))


(module+ test
  (test-case
      "Drawing the hold piece"
    (displayln "Drawing the hold piece")
    (hold-piece-pict 'I)))


;; [Listof shape-name/c] -> pict
(define (queue-pict sn-ls)
  (~> (apply vl-append BLOCK-W (map shape-pict sn-ls))
      (inset 10)))


(module+ test
  (test-case
      "Drawing the piece preview"
    (displayln "Drawing the piece preview")
    (define sn-ls0 '(I J))
    (queue-pict sn-ls0)))
