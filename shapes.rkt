#lang racket/base

(require racket/contract)
(require racket/lazy-require)
(require rackunit)
(require racket/generator)
(require racket/draw)
(require "utils.rkt")

(lazy-require ["block.rkt" (block block?)])


(provide
 SHAPE-COLOR
 (contract-out
  [SHAPE-NAMES (listof symbol?)]
  [shape-name? (-> any/c boolean?)]
  [shape-name->blocks (-> shape-name? rotation? (listof block?))]
  [shape-width (-> shape-name? natural-number/c)]
  [shape-gap-below (-> shape-name? natural-number/c)]
  [shape-generator? contract?]
  [kick-data (-> shape-name? rotation? rotation?
                 (listof (list/c integer? integer?)))]
  [7-loop-shape-generator shape-generator?]
  ))


;; ----------------------------
;; Definitions


(define SHAPE-NAMES
  '(L J S Z O I T))

; Rotation is one of '(0 1 2 3),
; meaning 0°, 90°, 180° and 270° of clockwise rotation respectively
(define (rotation? sexp)
  (not (not (member sexp '(0 1 2 3)))))


(define (shape-name? sexp)
  (not (not (member sexp SHAPE-NAMES))))


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(define SHAPE-COLOR
  (hash 'L (make-color 255 128 0)
        'J (make-color 0 132 255)
        'S (make-color 0 217 51)
        'Z (make-color 245 7 7)
        'T (make-color 205 7 245)
        'I (make-color 0 247 255)
        'O (make-color 242 235 12)
        'garbage (make-color 156 154 154)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shape matrices with pre-computed rotations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define h-shapes
  (make-immutable-hash '((L . ((#f #f #t)
                               (#t #t #t)
                               (#f #f #f)))
                         (J . ((#t #f #f)
                               (#t #t #t)
                               (#f #f #f)))
                         (Z . ((#t #t #f)
                               (#f #t #t)
                               (#f #f #f)))
                         (S . ((#f #t #t)
                               (#t #t #f)
                               (#f #f #f)))
                         (T . ((#f #t #f)
                               (#t #t #t)
                               (#f #f #f)))
                         (O . ((#t #t)
                               (#t #t)))
                         (I . ((#f #f #f #f)
                               (#t #t #t #t)
                               (#f #f #f #f)
                               (#f #f #f #f))))))


; ShapeName -> '((ShapeName rot-1) Shape-1
;                (ShapeName rot-2) Shape-2 ...) for every rot in Rotation
(define (shape-all-rotations shape-name)
  (let* ([shape (hash-ref h-shapes shape-name)]
         [shape90 (matrix-rotate-cw shape)]
         [shape180 (matrix-rotate-cw shape90)]
         [shape270 (matrix-rotate-cw shape180)])
    `(((,shape-name 0) . ,shape)
      ((,shape-name 1) . ,shape90)
      ((,shape-name 2) . ,shape180)
      ((,shape-name 3) . ,shape270))))


; Hash: '(ShapeName Rotation) -> Shape
; This is the final data structure to hold the piece shapes for every rotation
(define h-shapes-rot
  (make-immutable-hash
   (de-nest
    (map (λ (piece-name) (shape-all-rotations piece-name))
         SHAPE-NAMES))))


; ShapeName Rotation -> (Listof Block)
; Return the list of blocks a ShapeName is composed of,
; with its position at the origin (0, 0)
(define (shape-name->blocks shape-name [rotation 0])
  (define shape (hash-ref h-shapes-rot (list shape-name rotation)))
  (for/fold ([accum '()])
            ([y (in-naturals)]
             [row (reverse shape)])
    (append accum
            (for/list ([bool row]
                       [x (in-naturals)]
                       #:when bool)
              (block x y shape-name #f)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Next shape generators ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define shape-generator?
  (and/c generator? (-> shape-name?)))


;; Guarantees the order and starting shape
(define 7-loop-shape-generator
  (sequence->repeated-generator '(L J S Z O I T)))


;;;;;;;;;;;;;;;;;
;; Shape utils ;;
;;;;;;;;;;;;;;;;;


; ShapeName -> Natural
; Remember that a Shape is a matrix,
; so no matter the rotation, the width is the same
(define (shape-width shape-name)
  (length (hash-ref h-shapes shape-name)))


; ShapeName -> Natural
; We want to spawn each piece such that its bottom blocks are at level 21,
; so 21 minus this gap will be the position we want to spawn the piece at
(define (shape-gap-below shape-name)
  (define shape (hash-ref h-shapes-rot (list shape-name 0)))
  (for/first ([row (reverse shape)]
              [y (in-naturals)]
              #:when (member #t row))
    
    y))

(module+ test
  (test-case
      "shape-gap-below"
    (check-equal? (shape-gap-below 'L) 1)
    (check-equal? (shape-gap-below 'O) 0)
    (check-equal? (shape-gap-below 'I) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotation kicks data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


; A KickDataTable is a Hash:
; (Rotation . Rotation) -> List of (Pair of Integer)
; It maps the (initial rotation, final rotation) pair
; to the list of "kicks" to try when rotating a shape.

; Rotating a piece might fail because of a wall or a nearby piece,
; but it can succeed if you move left/right.
; The "kicks" is a movement we apply to a piece before rotating it,
; and a kick table is a sequence of kicks we try until one succeeds
; For details, see: https://tetris.wiki/Super_Rotation_System


; J, L, S, T, Z Tetromino KickDataTable
(define h-kick-data-1
  (make-immutable-hash
   '(((0 . 1) . (( 0  0) (-1  0) (-1 +1) ( 0 -2) (-1 -2)))
     ((1 . 0) . (( 0  0) (+1  0) (+1 -1) ( 0 +2) (+1 +2)))
     ((1 . 2) . (( 0  0) (+1  0) (+1 -1) ( 0 +2) (+1 +2)))
     ((2 . 1) . (( 0  0) (-1  0) (-1 +1) ( 0 -2) (-1 -2)))
     ((2 . 3) . (( 0  0) (+1  0) (+1 +1) ( 0 -2) (+1 -2)))
     ((3 . 2) . (( 0  0) (-1  0) (-1 -1) ( 0 +2) (-1 +2)))
     ((3 . 0) . (( 0  0) (-1  0) (-1 -1) ( 0 +2) (-1 +2)))
     ((0 . 3) . (( 0  0) (+1  0) (+1 +1) ( 0 -2) (+1 -2))))))


; I Tetromino KickDataTable
(define h-kick-data-2
  (make-immutable-hash
   '(((0 . 1) . (( 0  0) (-2  0) (+1  0) (-2 -1) (+1 +2)))
     ((1 . 0) . (( 0  0) (+2  0) (-1  0) (+2 +1) (-1 -2)))
     ((1 . 2) . (( 0  0) (-1  0) (+2  0) (-1 +2) (+2 -1)))
     ((2 . 1) . (( 0  0) (+1  0) (-2  0) (+1 -2) (-2 +1)))
     ((2 . 3) . (( 0  0) (+2  0) (-1  0) (+2 +1) (-1 -2)))
     ((3 . 2) . (( 0  0) (-2  0) (+1  0) (-2 -1) (+1 +2)))
     ((3 . 0) . (( 0  0) (+1  0) (-2  0) (+1 -2) (-2 +1)))
     ((0 . 3) . (( 0  0) (-1  0) (+2  0) (-1 +2) (+2 -1))))))


; ShapeName -> KickDataTable
(define h-kick-data
  (make-immutable-hash  `([I . ,h-kick-data-2]
                          [L . ,h-kick-data-1]
                          [J . ,h-kick-data-1]
                          [S . ,h-kick-data-1]
                          [Z . ,h-kick-data-1]
                          [O . ,h-kick-data-1]
                          [T . ,h-kick-data-1])))


; ShapeName Rotation Rotation -> (Listof (list integer integer))
; Returns the list of kicks to try for given shape when rotating fails
(define (kick-data sn rot-initial rot-final)
  (define ls (hash-ref (hash-ref h-kick-data sn)
                       (cons rot-initial rot-final)
                       #f))
  (if (not ls)
      (error (format "No kick data for rotating ~v from ~v to ~v." sn rot-initial rot-final))
      ls))

(module+ test
  (check-equal?
   (kick-data 'L 3 0)
   '(( 0  0) (-1  0) (-1 -1) ( 0 +2) (-1 +2))))


