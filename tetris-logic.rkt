#lang racket/base

(require lang/posn)
(require racket/function)
(require racket/list)
(require rackunit)
(require racket/lazy-require)
(require 2htdp/image)

(lazy-require ["tetris-draw.rkt" (draw-tetris)])

(provide
 WIDTH HEIGHT
 (struct-out tetris)
 (struct-out block)
 tetris-init
 tetris-blocks
 tetris-move
 tetris-on-tick)


;; Grid size
(define WIDTH 10)
(define HEIGHT 20)

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;


; Any List -> Bool
; returns True if value es in list
(define (member? el ls)
  (ormap [λ (s) (equal? s el)] ls))


; Function(Any -> Boolean) List -> Bool
; Returns true if any member of the list satisfies given function
; when passed as an argument to it
(define (any-satisfies? fun l)
  (ormap [λ (elem) (fun elem)] l))


; List -> Any
; Return a random item from the list
(define (random-choice l)
  (list-ref l (random (length l))))


; List of Lists -> List
; Flatten one level of nesting
(define (de-nest lss)
  (foldr
   (λ (ls xs)
     (foldr cons xs ls))
   '() lss))


; List of Lists -> List of Lists
; Transpose a matrix
(define (transpose ll)
  (apply map list ll))


; List of Lists -> List of Lists
; Rotate matrix 90 degrees
(define (rotate90 ll)
  (map reverse (transpose ll)))


;;;;;;;;;;;;
;; Blocks ;;
;;;;;;;;;;;;

; As a convention, the coordinates space will be the same as in math:
; x increases to the right
; y increases to the top
; so the (0, 0) coordinate is in the bottom-left corner


; A Block is a structure:
;   (make-block Posn)
;
; (make-block (make-posn x y)) depicts a block at position (x, y) in the grid,
; where (0, 0) marks the bottom-left position in the playfield
(define-struct block [posn color] #:transparent)


; A Piece is a structure:
;   (make-piece Posn ShapeName Rotation)
;
;(make-piece (make-posn x y) 'L 3)
;    Represents an L piece whose Shape's bottom-left corner is at coordinates (x, y)
;    and that is rotated 270 degrees.
; A Piece is used to represent the active piece that the players controls
(define-struct piece [posn shape-name rotation] #:transparent)

; A ShapeName is one of: '(L J S Z O T I)
(define shape-names '(L J Z S T O I))

; A Rotation is one of '(0 1 2 3),
; representing 0, 90, 180 and 270 clock-wise rotation, respectively


; Rotation, Or[RotateDirection / Rotation] -> Rotation
(define (rotation+ rot0 dirn)
  (let* ([rot-count
          (cond [(eq? 'cw dirn) 1]
                [(eq? 'ccw dirn) -1]
                [(integer? dirn) dirn]
                [else (error "Invalid rotation argument")])])
    (modulo (+ rot0 rot-count) 4)))
(check-equal? (rotation+ 0 1) 1)
(check-equal? (rotation+ 3 1) 0)
(check-equal? (rotation+ 0 -1) 3)
(check-equal? (rotation+ 0 'cw) 1)
(check-equal? (rotation+ 0 'ccw) 3)


; Piece, Or[Integer / RotateDirection] -> Piece
; Positive rot for clockwise, negative for counter-clockwise
(define (piece-rotate p rot)
  (let* ([rot-count
          (cond [(eq? 'cw rot) 1]
                [(eq? 'ccw rot) -1]
                [(integer? rot) rot]
                [else (error "Invalid rotation argument")])]
         [rot0 (piece-rotation p)]
         [new-rot (rotation+ rot0 rot)])
    (struct-copy piece p [rotation new-rot])))
(check-equal? (piece-rotate (make-piece (make-posn 1 2) 'L 0) -1)
              (make-piece (make-posn 1 2) 'L 3))


; Or(Tetris / Piece / Playfield) -> List of Blocks
; Convert any data into a list of Blocks
(define (tetris-blocks t)
  (cond
    [(tetris? t) (de-nest (list (piece-blocks (tetris-piece t))
                                (tetris-playfield t)))]
    [(piece? t) (piece-blocks (tetris-piece t))]
    [(list? t) t]
    [else '()]))


;;;;;;;;;;;;
;; Shapes ;;
;;;;;;;;;;;;

; A Shape is a List of Lists of Boolean
; A Piece occupies those blocks for which the Shape has a value of True
; The Shape's bottom-left corner coincides with the Piece's position

(define h-shape-color
  (make-immutable-hash '((L . orange)
                         (J . "Royal Blue")
                         (Z . red)
                         (S . green)
                         (T . "Light Purple")
                         (O . yellow)
                         (I . "Medium Cyan"))))

; We pre-compute all the shapes rotations for a slight optimization,
; and maybe as an exercise

; Hash: ShapeName -> Shape
(define pieces
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
(define (h-piece-rot shape-name)
  (let* ([shape (hash-ref pieces shape-name)]
         [shape90 (rotate90 shape)]
         [shape180 (rotate90 shape90)]
         [shape270 (rotate90 shape180)])
    `(((,shape-name 0) . ,shape)
      ((,shape-name 1) . ,shape90)
      ((,shape-name 2) . ,shape180)
      ((,shape-name 3) . ,shape270))))


; Hash: '(ShapeName Rotation) -> Shape
; This is the final data structure to hold the piece shapes for every rotation
(define h-pieces-rot
  (make-immutable-hash
   (de-nest
    (map (λ (piece-name) (h-piece-rot piece-name))
         shape-names))))


;;;;;;;;;;;;
;; Tetris ;;
;;;;;;;;;;;;


; A Tetris is a structure:
;   (make-tetris Piece Playfield)
; A Playfield is a list of Blocks
;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping Block, while b1, b2, and ... are the resting Blocks
(define-struct tetris (piece playfield)
  #:transparent)


;; Examples:
(define playfield0 `())
(define piece0 (make-piece (make-posn 5 (- HEIGHT 3)) 'L 0))
(define tetris0 (make-tetris piece0 playfield0))
(define playfield1 `(,(make-block (make-posn 0 0) 'gray)))


(define (spawn-piece)
  (make-piece (make-posn 5 (- HEIGHT 3))
              (random-choice shape-names)
              0))


; Number, Number -> List of Pairs
; Return all 2-combinations of 2 ranges of numbers
(define (combinations N M)
  (de-nest
   (map (λ (y) (map list
                         (build-list N identity)
                         (build-list N (λ (_) y))))
        (build-list M identity))))
(check-equal? (combinations 2 4)
              '((0 0) (1 0) (0 1) (1 1) (0 2) (1 2) (0 3) (1 3)))


; Matrix of Booleans -> List of Pairs of Integer
; The pairs represent the offset coordinates to apply to the Piece position
; with the origin at the bottom-left of the Shape
(define (shape-to-coords m)
  (let* ([len (length m)]
         [block-at?
          (λ (posn)
            (let* ([x (car posn)]
                   [y (second posn)]
                   [h (length m)])
              (list-ref (list-ref m (- h y 1)) x)))])
    (filter block-at? (combinations len len))))
(check-equal?  (andmap (λ (p) (member? p '((0 1) (1 1) (2 1) (2 2))))
                       (shape-to-coords (hash-ref pieces 'L)))
               #t)


; Piece -> List of Blocks
; Returns a list of blocks that the piece consists of
(define (piece-blocks piece)
  (let* ([sh-name (piece-shape-name piece)]
         [sh (hash-ref h-pieces-rot
                          `(,sh-name
                            ,(piece-rotation piece)))]
         [coords (shape-to-coords sh)]
         [sh-color (hash-ref h-shape-color sh-name)])
    (map (λ (coord)
           (make-block
            (make-posn (+ (first coord) (posn-x (piece-posn piece)))
                       (+ (second coord) (posn-y (piece-posn piece))))
            sh-color))
         coords)))
(check-equal? (map block-posn (piece-blocks (make-piece (make-posn 0 0) 'L 0)))
              (map block-posn `(,(make-block (make-posn 0 1) 'gray)
                                ,(make-block (make-posn 1 1) 'gray)
                                ,(make-block (make-posn 2 1) 'gray)
                                ,(make-block (make-posn 2 2) 'gray))))
(check-equal? (map block-posn (piece-blocks (make-piece (make-posn 1 5) 'J 0)))
              (map block-posn `(,(make-block (make-posn 1 6) 'gray)
                                ,(make-block (make-posn 2 6) 'gray)
                                ,(make-block (make-posn 3 6) 'gray)
                                ,(make-block (make-posn 1 7) 'gray))))


; Block Playfield -> Boolean
; Return true if block is overlapping with another block
; or if it's outside the playfield
(define (block-overlapping-playfield? b plf)
  (cond
    [(empty? plf) #f]
    [else (or (equal? (block-posn b) (block-posn (first plf)))
              (block-overlapping-playfield? b (rest plf)))]))
(check-equal?
 (block-overlapping-playfield? (make-block (make-posn 0 1) 'gray) (list (make-block (make-posn 0 0) 'gray)))
 #f)
(check-equal?
 (block-overlapping-playfield? (make-block (make-posn 0 0) 'gray) (list (make-block (make-posn 0 0) 'gray)))
 #t)


; Block Playfield -> Boolean
; Return true if block is outside the Playfield
(define (block-outside-playfield? b)
  (or (< (posn-x (block-posn b)) 0)
      (>= (posn-x (block-posn b)) WIDTH)
      (< (posn-y (block-posn b)) 0)
      (>= (posn-y (block-posn b)) HEIGHT)))
(check-equal? (block-outside-playfield? (make-block (make-posn -1 0) 'gray)) #t)
(check-equal? (block-outside-playfield? (make-block (make-posn WIDTH 0) 'gray)) #t)
(check-equal? (block-outside-playfield? (make-block (make-posn 0 HEIGHT) 'gray)) #t)
(check-equal? (block-outside-playfield? (make-block (make-posn 0 -1) 'gray)) #t)
(check-equal? (block-outside-playfield? (make-block (make-posn 0 0) 'gray)) #f)


; Block Playfield -> Boolean
; Return true if block is overlapping with another block
; or if it's outside the playfield
(define (block-overlapping? b plf)
  (or (block-outside-playfield? b)
      (block-overlapping-playfield? b plf)))


; Piece, Playfield -> Boolean
; Return true if any block in the piece is overlapping in the Playfield
(define (piece-overlapping? p plf)
  (ormap (λ (b)
           (block-overlapping? b plf))
         (piece-blocks p)))
(check-equal? (piece-overlapping? (make-piece (make-posn 5 5) 'L 0) '()) #f)
(check-equal? (piece-overlapping? (make-piece (make-posn 5 -2) 'L 0) '()) #t)


; Posn, Posn -> Posn
; Add Posns
(define (posn+ p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))
(check-equal? (posn+ (make-posn 1 2) (make-posn -1 1))
              (make-posn 0 3))


; Piece, Any[Direction / Posn / Pair of int] -> Piece
; Direction is one of: 'right, 'left, 'down, 'up
; Move the piece using either a symbol, a pair or a Posn
(define (piece-move p dirn)
  (let* ([delta-posn
          (cond [(posn? dirn) dirn]
                [(eq? 'right dirn) (make-posn 1 0)]
                [(eq? 'left dirn) (make-posn -1 0)]
                [(eq? 'down dirn) (make-posn 0 -1)]
                [(eq? 'up dirn) (make-posn 0 1)]
                [(and (list? dirn)
                      (= (length dirn) 2)
                      (integer? (first dirn))
                      (integer? (second dirn)))
                 (make-posn (first dirn) (second dirn))])])
    (struct-copy piece p [posn (posn+ (piece-posn p)
                                      delta-posn)])))


; Direction, Piece, Playfield -> Piece
; Try to move the piece in the specified Direction
; Return the moved piece if it could be moved,
; otherwise return the original piece
(define (try-move-piece dirn piece plf)
  (let* ([pposn (piece-posn piece)]
         [new-piece (piece-move piece dirn)]
         [overlapping (piece-overlapping? new-piece plf)])
    (if overlapping piece new-piece)))


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


; RotateDirection, Piece, Playfield -> Piece
; RotateDirection is one of ['cw 'ccw]
; Try to rotate the piece according to the Super Rotation System,
; return it if succeeded, otherwise return the original
(define (try-rotate-piece dirn piece0 plf)
  (let* ([shape-name (piece-shape-name piece0)]
         [rot0 (piece-rotation piece0)]
         [new-rot (rotation+ rot0 dirn)]
         [kick-table (hash-ref h-shape-name-to-kick-data shape-name)]
         [kick-list (hash-ref kick-table `(,rot0 . ,new-rot))]
         [try-kick  ; return #t if this kick doesn't fail, else #f
          (λ (k)
            (let* ([kicked-piece (piece-move piece0 k)]
                   [new-piece (piece-rotate kicked-piece dirn)]
                   [overlapping (piece-overlapping? new-piece plf)])
              (not overlapping)))]
         [memf-result (memf try-kick kick-list)])
    (if (list? memf-result)
        (let* ([posn0 (piece-posn piece0)]
               [kick (car memf-result)]
               [delta-posn (make-posn (car kick) (cadr kick))]
               [new-posn (posn+ posn0 delta-posn)])
          (struct-copy piece piece0
                       [posn new-posn]
                       [rotation new-rot]))
        piece0)))

; Test
[define p0 (make-piece (make-posn 0 2) 'J 0)]
[define p1 (make-piece (make-posn 1 0) 'J 3)]
[define plf (list (make-block (make-posn 0 0) 'gray)
                  (make-block (make-posn 0 2) 'gray)
                  (make-block (make-posn 2 4) 'gray))]
(check-equal? (try-rotate-piece 'ccw p0 plf) p1)

[define p2 (make-piece (make-posn 0 2) 'J 0)]
[define p3 (make-piece (make-posn -1 3) 'J 1)]
[define plf2 (list (make-block (make-posn 0 0) 'gray)
            (make-block (make-posn 0 2) 'gray)
            (make-block (make-posn 2 4) 'gray))]
(define rotate-test
  (beside (draw-tetris (make-tetris p2 plf2))
          (draw-tetris (make-tetris p3 plf2))))
(check-equal? (try-rotate-piece 'cw p2 plf2) p3)


; Piece, Playfield -> Piece
; Return the piece rotated a 2 times, but only if both times succeeded
; Otherwise return the original piece
(define (try-rotate-piece-180 p plf)
  (let* ([rotated1 (try-rotate-piece 'cw p plf)]
         [rotated2 (try-rotate-piece 'cw rotated1 plf)])
    (if (or (equal? p rotated1)
            (equal? rotated1 rotated2))
        p
        rotated2)))


; Piece Playfield -> Piece
; Return the Piece after it was moved down as much as possible
(define (soft-drop piece plf)
  (let* ([new-piece (try-move-piece 'down piece plf)]
         [same (equal? piece new-piece)])
    (if same piece
        (soft-drop new-piece plf))))
(check-equal? (soft-drop (make-piece (make-posn 5 (- HEIGHT 3)) 'L 0) '())
              (make-piece (make-posn 5 -1) 'L 0))



(define plf-full1 `(,@(build-list 10 (λ (x) (make-block (make-posn x 0) 'gray)))
                 ,(make-block (make-posn 5 1) 'gray)
                 ,@(build-list 10 (λ (x) (make-block (make-posn x 2) 'gray)))
                 ,(make-block (make-posn 4 3) 'blue)))

; Playfield -> List of Integers
; Returns a list of row indeces that represent completed lines
(define (complete-lines plf)
  (let* ([line-complete? ;;  Returns true if line with index y is complete
          (λ (y)
            (andmap (λ (x) (any-satisfies?
                                 (λ (b) (equal? (block-posn b) (make-posn x y)))
                                 plf))
                    (build-list WIDTH identity)))])
    (filter line-complete? (build-list HEIGHT identity))))
(check-equal? (complete-lines plf-full1)
              '(0 2))


; Number, List of Numbers -> Integer
; Count how many numbers in the list are smaller than the given number n
(define (count-less n l)
  (foldl + 0 (map (λ (x) (if (< x n) 1 0)) l)))
(check-equal? (count-less 5 '(0 2 5 6)) 2)


; Playfield -> Playfield
; Clear the completed tetris lines.
; the lines above the completed ones get shifted down.
(define (clear-lines plf)
  (let* ([complete-lines-list (complete-lines plf)]
         ;; List of blocks that remain after clearing the complete lines
         [remaining-blocks (filter
                            (λ (b) (not (member? (posn-y (block-posn b)) complete-lines-list)))
                            plf)]
         [new-y-fun (λ (b)
                      (- (posn-y (block-posn b))
                         (count-less
                          (posn-y (block-posn b))
                          complete-lines-list)))]
         ;; Move the block down as many lines as there are completed lines below
         [move-down-fun
          (λ (b) (struct-copy block b
                                   [posn (make-posn (posn-x (block-posn b))
                                                    (new-y-fun b))]))])
    (map move-down-fun remaining-blocks)))
; Test:
(define plf-cleared1 `(,(make-block (make-posn 5 0) 'gray)
                       ,(make-block (make-posn 4 1) 'blue)))
(check-equal? (clear-lines plf-full1) plf-cleared1)
; See this diagram for a visual explanation
;; (define clear-lines-explanation
;;   (beside (tetris-draw plf-full1)
;;           (text " Clear lines -> " 25 'black)
;;           (tetris-draw plf-cleared1)))


; Tetris -> Tetris
; Drop active Piece due to gravity,
; or if it landed, lock it, clear the lines, and spawn a new Piece
(define (drop-piece-or-lock tetris)
  (let* ([piece (tetris-piece tetris)]
         [plf (tetris-playfield tetris)]
         [new-piece (try-move-piece 'down piece plf)]
         [landed (equal? (piece-posn piece) (piece-posn new-piece))])
    (if landed
        (make-tetris (spawn-piece) (de-nest (list (piece-blocks piece) plf)))
        (make-tetris new-piece (tetris-playfield tetris)))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;


; _ -> Tetris
(define (tetris-init)
  (make-tetris (spawn-piece) '()))


; Tetris -> Tetris
(define (tetris-on-tick tetris)
  (let* ([t1 (drop-piece-or-lock tetris)]
         [b1 (tetris-piece t1)]
         [plf1 (tetris-playfield t1)]
         [plf-cleared (clear-lines plf1)])
    (make-tetris b1 plf-cleared)))


; Tetris, Move -> Tetris
; Move is one of '(left right cw ccw 180 soft-drop)
(define (tetris-move t mov)
  (let* ([p (tetris-piece t)]
         [plf (tetris-playfield t)]
         [new-piece
          (cond
            [(or (eq? mov 'left) (eq? mov 'right)) (try-move-piece mov p plf)]
            [(or (eq? mov 'cw) (eq? mov 'ccw)) (try-rotate-piece mov p plf)]
            [(eq? mov 180) (try-rotate-piece-180 p plf)]
            [(eq? mov 'soft-drop) (soft-drop p plf)]
            [else (error "Invalid argument for move.")])])
    (struct-copy tetris t [piece new-piece])))
