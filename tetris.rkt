#lang htdp/asl
;; (require racket/base)
(require 2htdp/universe)
(require 2htdp/image)

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
;    and that is rotated 270 degrees
;
; A ShapeName is one of: '(L J S Z O T I)
;
; A Rotation is one of '(0 1 2 3),
; representing 0, 90, 180 and 270 clock-wise rotation, respectively
(define-struct piece [posn shape-name rotation])


;;;;;;;;;;;;
;; Shapes ;;
;;;;;;;;;;;;

; A Shape is a List of Lists of Boolean
; A Piece occupies those blocks for which the Shape has a value of True
; The Shape's bottom-left corner coincides with the Piece's position

; We pre-compute all the shapes rotations for a slight optimization,
; and maybe as an exercise


; List of Lists -> List
; Flatten one level of nesting
(define (de-nest lss)
  (foldr
   (lambda (ls xs)
     (foldr cons xs ls))
   '() lss))

; Transpose a matrix (List of Lists
(define (transpose ll)
  (apply map list ll))

; Rotate matrix 90 degrees
(define (rotate90 ll)
  (map reverse (transpose ll)))


(define piece-names '(L J Z S T O I))

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
(define (piece-rot shape-name)
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
(define pieces-rot
  (make-immutable-hash
   (de-nest
    (map (lambda (piece-name) (piece-rot piece-name))
         piece-names))))


;;;;;;;;;;;;
;; Tetris ;;
;;;;;;;;;;;;


; A Tetris is a structure:
;   (make-tetris Block Playfield)
; A Playfield is a list of Blocks
;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping Block, while b1, b2, and ... are the resting Blocks
(define-struct tetris [block playfield])


;; Examples:
(define playfield0 '())
(define block-spawned (make-block (make-posn 5 (- HEIGHT 1))))
(define tetris0 (make-tetris block-spawned playfield0))

(define block1 (make-block (make-posn 5 0)))
(define tetris0-drop (make-tetris block1 playfield0))

(define tetris1 (make-tetris (make-block (make-posn 5 1))
                             (cons (make-block (make-posn 5 0)) '())))


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
(check-expect (piece-blocks (make-piece (make-posn 1 1) 'J 0))
              `(,(make-block (make-posn 1 2))
                ,(make-block (make-posn 2 2))
                ,(make-block (make-posn 3 2))
                ,(make-block (make-posn 1 3))))
(define (piece-blocks piece)
  (let* ([sh (hash-ref pieces-rot
                          `(,(piece-shape-name piece)
                            ,(piece-rotation piece)))]
         [coords (shape-to-coords sh)])
    (map (lambda (coord)
           (make-block
            (make-posn (+ (first coord) (posn-x (piece-posn piece)))
             (+ (second coord) (posn-x (piece-posn piece))))))
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
  (draw-blocks (cons (tetris-block tetris)
                     (tetris-playfield tetris))))


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


; Direction, Block, Playfield -> Block
; Direction is one of: 'right, 'left, 'down, 'rotate-cw
; The returned value is the moved block
; If the block can't be moved, the original block is returned
(define (move-block dir block plf)
  (let* ([new-block
          (cond
            [(eq? 'left dir)
             (make-block (make-posn (sub1 (posn-x (block-posn block))) (posn-y (block-posn block))))]
            [(eq? 'right dir)
             (make-block (make-posn (add1 (posn-x (block-posn block))) (posn-y (block-posn block))))]
            [(eq? 'down dir)
             (make-block (make-posn (posn-x (block-posn block)) (sub1 (posn-y (block-posn block)))))]
            [else block])]
         [overlapping (block-overlapping? new-block plf)])
    (if overlapping block new-block)))


; Block Playfield -> Block
; Return the block after it was moved down as much as possible
(check-expect (soft-drop (make-block (make-posn 5 (sub1 HEIGHT))) '())
              (make-block (make-posn 5 0)))
(check-expect (soft-drop (make-block (make-posn 5 (sub1 HEIGHT))) (list (make-block (make-posn 5 0))))
              (make-block (make-posn 5 1)))
(define (soft-drop block plf)
  (let* ([new-block (move-block 'down block plf)]
         [same (equal? block new-block)])
    (if same block
        (soft-drop new-block plf))))


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
(define clear-lines-explanation
  (beside (draw-tetris (make-tetris block-spawned plf-full1))
         (text " Clear lines -> " 25 'black)
         (draw-tetris (make-tetris block-spawned plf-cleared1))))
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
; Drop block due to gravity,
; or if it landed, lock it, clear the lines, and spawn a new block
(define (drop-block-or-lock tetris)
  (let* ([block (tetris-block tetris)]
         [plf (tetris-playfield tetris)]
         [new-block (move-block 'down block plf)]
         [landed (equal? (block-posn block) (block-posn new-block))])
    (if landed
        (make-tetris (make-block (make-posn 5 (sub1 HEIGHT))) (cons block plf))
        (make-tetris new-block (tetris-playfield tetris)))))


; Tetris -> Tetris
(define (tetris-on-tick tetris)
  (let* ([t1 (drop-block-or-lock tetris)]
         [b1 (tetris-block t1)]
         [plf1 (tetris-playfield t1)]
         [plf-cleared (clear-lines plf1)])
    (make-tetris b1 plf-cleared)))


; Tetris, Key -> Tetris
(define (tetris-on-key w k)
  (let* ([b (tetris-block w)]
         [plf (tetris-playfield w)])
    (cond
     [(key=? k " ")
      (make-tetris (soft-drop b plf) plf)]
     [(key=? k "left")
      (make-tetris (move-block 'left b plf) plf)]
     [(key=? k "right")
      (make-tetris (move-block 'right b plf) plf)]
     [else w])))


(define (main x)
  (big-bang tetris0
            [on-tick tetris-on-tick 0.1]
            [to-draw draw-tetris]
            [on-key tetris-on-key]))
