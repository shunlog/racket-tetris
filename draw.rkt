#lang racket/base

(require racket/list)
(require threading)
(require racket/gui/base)
(require lang/posn)
(require pict)
(require memo)

(require "playfield.rkt")
(require "block.rkt")
(require "shapes.rkt")
(require "draw-utils.rkt")
(require "tetrion.rkt")
(require "utils.rkt")

(define BLOCK-W 32)
(define GARBAGE-COLOR (make-color 156 154 154))
(define GHOST-ALPHA 0.3)
(define VANISH-LINES 2)    ; number of rows to draw in the vanish zone

(provide
 BLOCK-W
 playfield-pict
 queue-pict
 hold-piece-pict
 )


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


;; BlockType -> Color
(define (block-color bt)
  (cond
    [(equal? 'garbage bt) GARBAGE-COLOR]
    [else
     (define shape-color (get-shape-color (car bt)))
     (cond [(equal? 'normal (cdr bt)) shape-color]
           [(equal? 'ghost (cdr bt)) (set-alpha shape-color GHOST-ALPHA)])]))


(define (block-type-pict bt)
  (define color (block-color bt))
  (define hw (/ BLOCK-W 2))             ; half width of square
  (define bw 2)                         ; border width inside square
  (define lt (lighter color .2))
  (define dk (darker color .08))
  (define inner-rects (pin-under
                       (rectangle hw hw
                                  #:border-color dk
                                  #:border-width bw)
                       1 1
                       (rectangle hw hw
                                  #:border-color lt
                                  #:border-width bw))
    )
  (define outer-rects (pin-under
                       (rectangle BLOCK-W BLOCK-W
                                  #:border-color dk
                                  #:border-width bw)
                       1 1
                       (rectangle BLOCK-W BLOCK-W
                                  #:border-color lt
                                  #:border-width bw)))
  (cond
    [(equal? bt 'garbage)
     (filled-rectangle BLOCK-W BLOCK-W
                       #:color color
                       #:border-color dk
                       #:border-width bw)]

    [(equal? (cdr bt) 'normal) (cc-superimpose
                                (filled-rectangle BLOCK-W BLOCK-W
                                                  #:color color
                                                  #:draw-border? #f)
                                outer-rects
                                inner-rects)]
    [else (filled-rectangle BLOCK-W BLOCK-W
                            #:color color
                            #:draw-border? #f)])
  )

;; Use hash so it uses (equal?) to compare values,
;; Otherwise comparing BlockTypes (which are cons) would return false
(define/memoize (block-type-bitmap bt ) #:hash hash
  (bitmap (block-type-pict bt)))

(define (playfield-pict plf)
  (define rows (playfield-rows plf))
  (define cols (playfield-cols plf))
  (define bl (playfield-block-matrix plf))
  (define bt-ls (de-nest (reverse (take bl (+ VANISH-LINES rows)))))
  (ct-superimpose
   (filled-rectangle (* BLOCK-W cols) (* BLOCK-W (+ VANISH-LINES rows))
                     #:color "black")
   (rectangle (* cols BLOCK-W) (* VANISH-LINES BLOCK-W)
              #:border-color "gray"
              #:border-width 3)
   (table cols (map (λ (bt)
                      (if (not bt) (blank BLOCK-W BLOCK-W) (block-type-bitmap bt)))
                    bt-ls)
          cc-superimpose cc-superimpose 0 0)))

(module+ test
  (test-case
      "Drawing a small Playfield"
    (displayln "Drawing a small Playfield")
    (define plf0
      (~> (empty-playfield 3 3)
          (playfield-add-blocks
           (strings->blocks '(".I" "J.")))))
    (playfield-pict plf0))

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
    (playfield-pict plf0))

  

  
  (test-case
      "Time drawing a large playfield"
    (define (time-drawing-playfield rows cols)
      (define large-tion
        (~> (new-tetrion #:rows rows #:cols cols)
            (tetrion-add-garbage rows)))
      (define plf (tetrion-playfield large-tion))
      (time (playfield-pict plf )))
    (for ([rows '(10 100 100)]
          [cols '(10 10 100)])
      (display (format "~a blocks: " (* rows cols)))
      (time-drawing-playfield rows cols)))
  )


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
    (pin-over img dx dy (block-type-bitmap (cons sn 'normal))))
  )

(module+ test
  (test-case
      "Drawing a shape"
    (displayln "Drawing a shape")
    (shape-pict 'L)))


;; (or/c #f shape-name) -> pict
(define (hold-piece-pict sn)
  (define bg (blank (* 5 BLOCK-W) (* 5 BLOCK-W)))
  (define piece (cond
                  [(not sn) bg]
                  [else (cc-superimpose bg (shape-pict sn))]))
  (~> piece
      (frame #:line-width 2)))


(module+ test
  (test-case
      "Drawing the hold piece"
    (displayln "Drawing the hold piece")
    (hold-piece-pict 'I)))


;; [Listof shape-name/c] -> pict
;; We'll assume max. piece height to be 2 blocks,
;; and max. width to be 4 blocks.
;; We want to ensure the pict will always be of the same size,
;; even if sometimes the pieces won't occupy it all.
(define (queue-pict sn-ls)
  (define border 10)
  (define size (length sn-ls))
  (define w (+ (* border 2) (* 4 BLOCK-W)))
  (define h (+ (* border 2) (* (+ (* 2 size) (- size 1))
                               BLOCK-W)))
  (~> (apply vl-append BLOCK-W (map shape-pict sn-ls))
      (ct-superimpose (blank w h) _)
      (inset border)
      (frame #:line-width 2)))


(module+ test
  (test-case
      "Drawing the piece preview"
    (displayln "Drawing the piece preview")
    (define sn-ls0 '(I J))
    (queue-pict sn-ls0)))
