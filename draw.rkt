#lang racket/base

(require racket/list)
(require threading)

(require lang/posn)
(require pict)
(require memo)
(require racket/contract)
(require racket/class)
(require racket/draw)

(require "playfield.rkt")
(require "block.rkt")
(require "shapes.rkt")
(require "draw-utils.rkt")
(require "tetrion.rkt")
(require "utils.rkt")


(provide
 BLOCK-W
 tile-pict
 playfield-pict
 queue-pict
 hold-piece-pict
 tetrion-pict
 )

(define (make-color r g b) (make-object color% r g b))

(define GARBAGE-COLOR (make-color 156 154 154))
(define DEFAULT-COLOR (make-color 255 200 255))

(define COLORS-HASH
  (hash 'L (make-color 255 128 0)
        'J (make-color 0 132 255)
        'S (make-color 0 217 51)
        'Z (make-color 245 7 7)
        'T (make-color 205 7 245)
        'I (make-color 0 247 255)
        'O (make-color 242 235 12)))

(define BLOCK-W 22)

(define GHOST-ALPHA 0.3)
(define VANISH-LINES 2)    ; number of rows to draw in the vanish zone


(define/contract (get-shape-color shape-name)
  (-> shape-name/c (is-a?/c color%))
  (hash-ref COLORS-HASH shape-name DEFAULT-COLOR))



; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))



;; Tile -> Color
(define/contract (block-color tile)
  (-> tile? (is-a?/c color%))
  (cond
    [(tile-garbage? tile) GARBAGE-COLOR]
    [else
     (define shape-color (get-shape-color (tile-shape tile)))
     (cond [(tile-normal? tile) shape-color]
           [(tile-ghost? tile) (set-alpha shape-color GHOST-ALPHA)]
           [else (raise "Error")])]))


(define (tile-pict-v0 tile)
  (define color (block-color tile))
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
    [(tile-garbage? tile)
     (filled-rectangle BLOCK-W BLOCK-W
                       #:color color
                       #:border-color dk
                       #:border-width bw)]

    [(tile-normal? tile) (cc-superimpose
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
;; Otherwise comparing Tiles (which are cons) would return false
(define/memoize (tile-bitmap tile ) #:hash hash
  (bitmap (if (not tile)
              (blank BLOCK-W BLOCK-W)
              (tile-pict-v0 tile))))

(define (playfield-pict plf)
  (define rows (playfield-rows plf))
  (define cols (playfield-cols plf))
  (define bl (playfield-block-matrix plf))
  (define tile-ls (de-nest (reverse (take bl (+ VANISH-LINES rows)))))
  (ct-superimpose
   (filled-rectangle (* BLOCK-W cols) (* BLOCK-W (+ VANISH-LINES rows))
                     #:color "black")
   (rectangle (* cols BLOCK-W) (* VANISH-LINES BLOCK-W)
              #:border-color "gray"
              #:border-width 3)
   (table cols (map (λ (tile) (tile-bitmap tile)) tile-ls)
          cc-superimpose cc-superimpose 0 0)))

;; Tetrion -> Pict
(define (tetrion-pict tn)
  (ht-append (playfield-pict (tetrion-playfield tn))
             (queue-pict (tetrion-queue tn))
             (hold-piece-pict (tetrion-on-hold tn))))

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
    (pin-over img dx dy (tile-bitmap (tile-normal sn))))
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


;;; 
;;; Get tile bitmaps from file
;;;

(define skin-bmp
  (make-object bitmap% "jstris_skin.png" 'unknown #f #t))
(define skin-pict (bitmap skin-bmp))

(define gap 1)
(define width (pict-width skin-pict))
(define height (pict-height skin-pict)) ;assuming no gaps on top/bottom

;;; Split tilemap into tile picts
(define tiles-pl
  (for/list
    [(i (modulo width height))]
   (define left-inset (* (+ height gap) i))
   (define right-inset (- width left-inset height))
   (inset/clip skin-pict
               (- left-inset) 0 (- right-inset) 0)))

(define (tile-pict-v1 tile)
  (cond
    [(tile-ghost? tile) (list-ref tiles-pl 7)]
    [(tile-garbage? tile) (list-ref tiles-pl 8)]
    [else (case (tile-shape tile)
            [(Z) (list-ref tiles-pl 0)]
            [(L) (list-ref tiles-pl 1)]
            [(O) (list-ref tiles-pl 2)]
            [(S) (list-ref tiles-pl 3)]
            [(I) (list-ref tiles-pl 4)]
            [(J) (list-ref tiles-pl 5)]
            [(T) (list-ref tiles-pl 6)]
            [else (list-ref tiles-pl 10)])]))


(module+ test
  (test-case
      "The tiles skin read from the file"
    (displayln "The tiles skin read from the file")
    tiles-pl))


;;; 
;;; Combine multiple versions

;;; Tile -> Pict
(define (tile-pict tile)
  (tile-pict-v1 tile))
