#lang htdp/asl
;; (require racket/base)
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;


; List -> Any
; Return a random item from the list
(define (random-choice l)
  (list-ref l (random (length l))))


; List of Lists -> List
; Flatten one level of nesting
(define (de-nest lss)
  (foldr
   (lambda (ls xs)
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

; image of a Block
(define BLOCK-SIZE 20) ; blocks are squares
(define BLOCK
  (overlay
   (square (- BLOCK-SIZE 1) "solid" "gray")
   (square BLOCK-SIZE "outline" "black")))

;; Grid size
(define WIDTH 10)
(define HEIGHT 20)
(define PLAYFIELD-WIDTH (* WIDTH BLOCK-SIZE))
(define PLAYFIELD-HEIGHT (* HEIGHT BLOCK-SIZE))

; image of empty Playfield
(define EMPTY-PLAYFIELD (empty-scene PLAYFIELD-WIDTH PLAYFIELD-HEIGHT))

; A Block is a structure:
;   (make-block Posn)
;
; (make-block (make-posn x y)) depicts a block at position (x, y) in the grid,
; where (0, 0) marks the bottom-left position in the playfield
(define-struct block [posn])


; A Piece is a structure:
;   (make-piece Posn ShapeName Rotation)
;
;(make-piece (make-posn x y) 'L 3)
;    Represents an L piece whose Shape's bottom-left corner is at coordinates (x, y)
;    and that is rotated 270 degrees.
; A Piece is used to represent the active piece that the players controls
(define-struct piece [posn shape-name rotation])

; A ShapeName is one of: '(L J S Z O T I)
(define shape-names '(L J Z S T O I))

; A Rotation is one of '(0 1 2 3),
; representing 0, 90, 180 and 270 clock-wise rotation, respectively

; Piece, Integer -> Piece
; Positive rot for clockwise, negative for counter-clockwise
(check-expect (piece-rotate (make-piece (make-posn 1 2) 'L 0) -1)
              (make-piece (make-posn 1 2) 'L 3))
(define (piece-rotate piece rot)
  (make-piece (piece-posn piece)
              (piece-shape-name piece)
              (modulo (+ rot (piece-rotation piece))
                      4)))

;;;;;;;;;;;;
;; Shapes ;;
;;;;;;;;;;;;

; A Shape is a List of Lists of Boolean
; A Piece occupies those blocks for which the Shape has a value of True
; The Shape's bottom-left corner coincides with the Piece's position

; We pre-compute all the shapes rotations for a slight optimization,
; and maybe as an exercise


; Hash: ShapeName -> Shape
(define pieces
  (make-immutable-hash '((L ((#f #f #t)
                             (#t #t #t)
                             (#f #f #f)))
                         (J ((#t #f #f)
                             (#t #t #t)
                             (#f #f #f)))
                         (Z ((#t #t #f)
                             (#f #t #t)
                             (#f #f #f)))
                         (S ((#f #t #t)
                             (#t #t #f)
                             (#f #f #f)))
                         (T ((#f #t #f)
                             (#t #t #t)
                             (#f #f #f)))
                         (O ((#t #t)
                             (#t #t)))
                         (I ((#f #f #f #f)
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
    `(((,shape-name 0) ,shape)
      ((,shape-name 1) ,shape90)
      ((,shape-name 2) ,shape180)
      ((,shape-name 3) ,shape270))))


; Hash: '(ShapeName Rotation) -> Shape
; This is the final data structure to hold the piece shapes for every rotation
(define h-pieces-rot
  (make-immutable-hash
   (de-nest
    (map (lambda (piece-name) (h-piece-rot piece-name))
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
(define-struct tetris [piece playfield])


;; Examples:
(define playfield0 `(,(make-block (make-posn 0 0))))
(define piece0 (make-piece (make-posn 5 (- HEIGHT 3)) 'L 0))
(define tetris0 (make-tetris piece0 playfield0))


(define (spawn-piece)
  (make-piece (make-posn 5 (- HEIGHT 3))
              (random-choice shape-names)
              0))


;; Block, Image -> Image
;; Draw a Block onto the playfield image
(define (draw-block b scene)
  (underlay/xy scene
               (* (posn-x (block-posn b)) BLOCK-SIZE)
               (- PLAYFIELD-HEIGHT
                  (* (+ (posn-y (block-posn b)) 1) BLOCK-SIZE))
               BLOCK))


; Number, Number -> List of Pairs
; Return all 2-combinations of 2 ranges of numbers
(check-expect (combinations 2 4)
              '((0 0) (1 0) (0 1) (1 1) (0 2) (1 2) (0 3) (1 3)))
(define (combinations N M)
  (de-nest
   (map (lambda (y) (map list
                         (build-list N identity)
                         (build-list N (lambda (_) y))))
        (build-list M identity))))


; Matrix of Booleans -> List of Pairs of Integer
; The pairs represent the offset coordinates to apply to the Piece position
; with the origin at the bottom-left of the Shape
(check-expect  (andmap (lambda (p) (member? p '((0 1) (1 1) (2 1) (2 2))))
                       (shape-to-coords (hash-ref pieces 'L)))
               #t)
(define (shape-to-coords m)
  (let* ([len (length m)]
         [block-at?
          (lambda (posn)
            (let* ([x (car posn)]
                   [y (second posn)]
                   [h (length m)])
              (list-ref (list-ref m (- h y 1)) x)))])
    (filter block-at? (combinations len len))))


; Piece -> List of Blocks
; Returns a list of blocks that the piece consists of
(check-expect (piece-blocks (make-piece (make-posn 0 0) 'L 0))
              `(,(make-block (make-posn 0 1))
                ,(make-block (make-posn 1 1))
                ,(make-block (make-posn 2 1))
                ,(make-block (make-posn 2 2))))
(check-expect (piece-blocks (make-piece (make-posn 1 5) 'J 0))
              `(,(make-block (make-posn 1 6))
                ,(make-block (make-posn 2 6))
                ,(make-block (make-posn 3 6))
                ,(make-block (make-posn 1 7))))
(define (piece-blocks piece)
  (let* ([sh (hash-ref h-pieces-rot
                          `(,(piece-shape-name piece)
                            ,(piece-rotation piece)))]
         [coords (shape-to-coords sh)])
    (map (lambda (coord)
           (make-block
            (make-posn (+ (first coord) (posn-x (piece-posn piece)))
             (+ (second coord) (posn-y (piece-posn piece))))))
         coords)))


; List of Blocks -> Image
; Draw the list of Blocks on the Playfield
(define (draw-blocks blocks)
  (foldl draw-block EMPTY-PLAYFIELD
         blocks))


; Piece, Image -> Image
; Draw a Piece onto the Playfield image
(define (draw-piece p)
  (draw-blocks (piece-blocks p)))


;; Tetris -> Image
;; Draw a Tetris playfieled
(define (draw-tetris tetris)
  (draw-blocks (de-nest (list (piece-blocks (tetris-piece tetris))
                              (tetris-playfield tetris)))))


; Block Playfield -> Boolean
; Return true if block is overlapping with another block
; or if it's outside the playfield
(check-expect
 (block-overlapping-playfield? (make-block (make-posn 0 1)) (list (make-block (make-posn 0 0))))
 #f)
(check-expect
 (block-overlapping-playfield? (make-block (make-posn 0 0)) (list (make-block (make-posn 0 0))))
 #t)
(define (block-overlapping-playfield? b plf)
  (cond
    [(empty? plf) #f]
    [else (or (equal? (block-posn b) (block-posn (first plf)))
              (block-overlapping-playfield? b (rest plf)))]))


; Block Playfield -> Boolean
; Return true if block is outside the Playfield
(check-expect (block-outside-playfield? (make-block (make-posn -1 0))) #t)
(check-expect (block-outside-playfield? (make-block (make-posn WIDTH 0))) #t)
(check-expect (block-outside-playfield? (make-block (make-posn 0 HEIGHT))) #t)
(check-expect (block-outside-playfield? (make-block (make-posn 0 -1))) #t)
(check-expect (block-outside-playfield? (make-block (make-posn 0 0))) #f)
(define (block-outside-playfield? b)
  (or (< (posn-x (block-posn b)) 0)
      (>= (posn-x (block-posn b)) WIDTH)
      (< (posn-y (block-posn b)) 0)
      (>= (posn-y (block-posn b)) HEIGHT)))


; Block Playfield -> Boolean
; Return true if block is overlapping with another block
; or if it's outside the playfield
(define (block-overlapping? b plf)
  (or (block-outside-playfield? b)
      (block-overlapping-playfield? b plf)))


; Piece, Playfield -> Boolean
; Return true if any block in the piece is overlapping in the Playfield
(check-expect (piece-overlapping? (make-piece (make-posn 5 5) 'L 0) '()) #f)
(check-expect (piece-overlapping? (make-piece (make-posn 5 -2) 'L 0) '()) #t)
(define (piece-overlapping? p plf)
  (ormap (lambda (b)
           (block-overlapping? b plf))
         (piece-blocks p)))


; Posn, Posn -> Posn
; Add Posns
(check-expect (posn+ (make-posn 1 2) (make-posn -1 1))
              (make-posn 0 3))
(define (posn+ p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))


; Piece, Direction -> Piece
; Direction is one of: 'right, 'left, 'down, 'rotate-cw
(define (move-piece piece dirn)
  (let* ([pposn (piece-posn piece)]
         [move-by-posn (lambda (psn)
                         (make-piece (posn+ pposn psn)
                                     (piece-shape-name piece)
                                     (piece-rotation piece)))])
    (cond
      [(eq? 'left dirn) (move-by-posn (make-posn -1 0))]
      [(eq? 'right dirn) (move-by-posn (make-posn 1 0))]
      [(eq? 'down dirn) (move-by-posn (make-posn 0 -1))]
      [(eq? 'rotate-cw dirn) (piece-rotate piece 1)]
      [(eq? 'rotate-ccw dirn) (piece-rotate piece -1)]
      [(eq? 'rotate-180 dirn) (piece-rotate piece 2)]
      [else piece])))


; Direction, Piece, Playfield -> Piece
; The returned value is the moved piece if it could be moved,
; otherwise it's the original piece
(define (move-piece-maybe dirn piece plf)
  (let* ([pposn (piece-posn piece)]
         [new-piece (move-piece piece dirn)]
         [overlapping (piece-overlapping? new-piece plf)])
    (if overlapping piece new-piece)))


; Block Playfield -> Block
; Return the block after it was moved down as much as possible
(check-expect (soft-drop (make-piece (make-posn 5 (- HEIGHT 3)) 'L 0) '())
              (make-piece (make-posn 5 -1) 'L 0))
(define (soft-drop piece plf)
  (let* ([new-piece (move-piece-maybe 'down piece plf)]
         [same (equal? piece new-piece)])
    (if same piece
        (soft-drop new-piece plf))))


; Playfield -> List of Integers
; Returns a list of row indeces that represent completed lines
(check-expect (complete-lines plf-full1)
              '(0 2))
(define (complete-lines plf)
  (let* ([line-complete? ;;  Returns true if line with index y is complete
          (lambda (y)
            (andmap (lambda (x) (member? (make-block (make-posn x y)) plf))
                    (build-list WIDTH identity)))])
    (filter line-complete? (build-list HEIGHT identity))))


; Number, List of Numbers -> Integer
; Count how many numbers in the list are smaller than the given number n
(check-expect (count-less 5 '(0 2 5 6)) 2)
(define (count-less n l)
  (foldl + 0 (map (lambda (x) (if (< x n) 1 0)) l)))


; Playfield -> Playfield
; Clear the completed tetris lines.
; the lines above the completed ones get shifted down.
(define plf-full1 `(,@(build-list 10 (lambda (x) (make-block (make-posn x 0))))
                 ,(make-block (make-posn 5 1))
                 ,@(build-list 10 (lambda (x) (make-block (make-posn x 2))))
                 ,(make-block (make-posn 4 3))))
(define plf-cleared1 `(,(make-block (make-posn 5 0))
                       ,(make-block (make-posn 4 1))))
; see this diagram for a visual explanation
;; (define clear-lines-explanation
;;   (beside (draw-tetris (make-tetris block-spawned plf-full1))
;;          (text " Clear lines -> " 25 'black)
;;          (draw-tetris (make-tetris block-spawned plf-cleared1))))
(check-expect (clear-lines plf-full1) plf-cleared1)
(define (clear-lines plf)
  (let* ([complete-lines-list (complete-lines plf)]
         ;; List of blocks that remain after clearing the complete lines
         [remaining-blocks (filter
                            (lambda (b) (not (member? (posn-y (block-posn b)) complete-lines-list)))
                            plf)]
         ;; Move the block down as many lines as there are completed lines below
         [move-down-fun
          (lambda (b)
            (make-block (make-posn (posn-x (block-posn b))
                         (- (posn-y (block-posn b))
                            (count-less (posn-y (block-posn b)) complete-lines-list)))))])
    (map move-down-fun remaining-blocks)))


; Tetris -> Tetris
; Drop active Piece due to gravity,
; or if it landed, lock it, clear the lines, and spawn a new Piece
(define (drop-piece-or-lock tetris)
  (let* ([piece (tetris-piece tetris)]
         [plf (tetris-playfield tetris)]
         [new-piece (move-piece-maybe 'down piece plf)]
         [landed (equal? (piece-posn piece) (piece-posn new-piece))])
    (if landed
        (make-tetris (spawn-piece) (de-nest (list (piece-blocks piece) plf)))
        (make-tetris new-piece (tetris-playfield tetris)))))


; Tetris -> Tetris
(define (tetris-on-tick tetris)
  (let* ([t1 (drop-piece-or-lock tetris)]
         [b1 (tetris-piece t1)]
         [plf1 (tetris-playfield t1)]
         [plf-cleared (clear-lines plf1)])
    (make-tetris b1 plf-cleared)))


; Tetris, Key -> Tetris
(define (tetris-on-key w k)
  (let* ([piece (tetris-piece w)]
         [plf (tetris-playfield w)])
    (cond
     [(key=? k " ")
      (make-tetris (soft-drop piece plf) plf)]
     [(key=? k "left")
      (make-tetris (move-piece-maybe 'left piece plf) plf)]
     [(key=? k "right")
      (make-tetris (move-piece-maybe 'right piece plf) plf)]
     [(or (key=? k "up") (key=? k "x"))
      (make-tetris (move-piece-maybe 'rotate-cw piece plf) plf)]
     [(key=? k "z")
      (make-tetris (move-piece-maybe 'rotate-ccw piece plf) plf)]
     [(key=? k "a")
      (make-tetris (move-piece-maybe 'rotate-180 piece plf) plf)]
     [else w])))


(define (main tick-durn)
  (big-bang tetris0
            [on-tick tetris-on-tick tick-durn]
            [to-draw draw-tetris]
            [on-key tetris-on-key]))
