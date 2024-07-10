#lang racket/base

(require "utils.rkt")

(provide
 shape-names
 h-shapes-rot
 h-shape-name-to-kick-data)

; A Shape is a square matrix (List of Lists) of Boolean values

; A ShapeName is an identifier used in the h-shapes Hash
(define shape-names '(L J Z S T O I))

; A Piece occupies those blocks for which the Shape has a value of True
; The Shape's bottom-left corner coincides with the Piece's position

; We pre-compute all the shapes rotations for a slight optimization,
; and maybe as an exercise

; Hash: ShapeName -> Shape
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computed all rotations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
         shape-names))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotation kicks data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


; A KickDataTable is a Hash:
; (Rotation . Rotation) -> List of (Pair of Integer)
; Interpretation:
;     map the (initial rotation, final rotation) pair
;     to the list of "kicks" to try when rotating a shape.
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
(define h-shape-name-to-kick-data
  (make-immutable-hash  `([I . ,h-kick-data-2]
                          [L . ,h-kick-data-1]
                          [J . ,h-kick-data-1]
                          [S . ,h-kick-data-1]
                          [Z . ,h-kick-data-1]
                          [O . ,h-kick-data-1]
                          [T . ,h-kick-data-1])))

