#lang racket/base

(require lang/posn)
(require racket/function)
(require racket/list)
(require rackunit)
(require racket/lazy-require)
(require 2htdp/image)
(require 2htdp/universe)
(require threading)

(require "utils.rkt")
(require "tetris-shapes-data.rkt")
(lazy-require ["tetris-draw.rkt"
               (draw-blocks)])

(provide
 WIDTH HEIGHT
 (struct-out tetris)
 (struct-out block)
 (struct-out piece)
 tetris-init
 tetris-on-key
 tetris-on-release
 tetris-on-tick
 tetris-ghost-blocks
 draw-any-blocks)



;;;;;;;;;;;;;;;;;;;
;; Configuration ;;
;;;;;;;;;;;;;;;;;;;


;; Grid size
(define WIDTH 10)
(define HEIGHT 20)

(define T-DELAY-AUTOSHIFT 200)          ; ms to start autoshifting
(define T-AUTOSHIFT-RATE 35)            ; ms between autoshifts
(define T-DROP-RATE 500)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;


; _ -> Tetris
(define (tetris-init time)
  [define p (spawn-piece)]
  (struct-copy tetris tetris0
               [piece p]
               [playfield '()]
               [ghostY 0]
               [t-start-dirn time]
               [t-last-autoshift time]
               [t-last-drop time]))


; Tetris -> Tetris
(define (tetris-on-tick t time)
  (~> t
      (tetris-tick-drop time)
      (tetris-tick-autoshift time)))


; Tetris, Key -> Tetris
(define (tetris-on-release t0 k ms)
  (tetris-update-key-state t0 k #f))


; Tetris, Key -> Tetris
(define (tetris-on-key t k ms)
  (if (tetris-key-pressed? t k)         ; ignore key auto-repeat
      t
      (~> t
          (tetris-update-t-start-dirn k ms)
          (tetris-update-last-dirn k)
          (tetris-update-key-state k #t)
          (tetris-key-just-pressed k ms))))


; List of [Piece / Block / Tetris] -> Image
; Draw a list of anything that can be converted to blocks
(define (draw-any-blocks ls)
  (draw-blocks (convert-to-blocks ls)))


;;;;;;;;;;;;;;;;;
;; Definitions ;;
;;;;;;;;;;;;;;;;;


; As a convention, the coordinates space will be the same as in math:
; x increases to the right
; y increases to the top
; so the (0, 0) coordinate is in the bottom-left corner


; A Block is a structure:
;   (make-block Posn ColorType)
; ColorType is a ShapeName or 'ghost
;
; (make-block (make-posn x y) 'L) depicts a block at position (x, y) in the grid,
;     where (0, 0) marks the bottom-left position in the playfield
;     and that should be colored like the L shape
(define-struct block [posn color] #:transparent)

; A Rotation is one of '(0 1 2 3),
; representing 0, 90, 180 and 270 clock-wise rotation, respectively

(define-struct tetris
  (piece
   playfield   
   ghostY
   h-key-state
   last-dirn-right?
   t-start-dirn
   t-last-autoshift
   t-last-drop)
  #:transparent)

; A Tetris is a structure:
;     KeysState keeps track whether "down", "left" and "right" are pressed

; * Playfield is a list of Blocks representing the resting blocks.

; * A Piece is a structure used to represent the active piece that the players controls
;   (make-piece Posn ShapeName Rotation)
;   E.g. (make-piece (make-posn x y) 'L 3)
;   Represents an L piece whose Shape's bottom-left corner is at coordinates (x, y)
;   and that is rotated 270 degrees.
(define-struct piece [posn shape-name rotation] #:transparent)

; * GhostY is an Integer
; representing the height at which to draw the ghost of the active piece.
; Keeping GhostY in the state is necessary for optimal drawing the ghost piece,
; otherwise we'd have to compute it on every tick, which is a bit wasteful.
; However, it also means we must be careful to always update GhostY
; whenever we move or rotate the piece, except when we drop it down.

; * KeysState is a hash that keeps track of the states of a few keys.
; The hash keys are key names, and the value is either #t or #f.
; If a key is not in the hash, it should be considered #f.

; * last-dirn-right? is a Boolean.
; if it's True, it means the last pressed direction was "right",
; otherwise the last one was "left".
; This is needed to make autoshift work properly when both keys are pressed.

; * t-start-dirn is the time in milliseconds when the side movement started,
; meaning when a direction key was pressed after both were released.
; As long as at least one direction key is held, this timer is not reset.
;
; If long enough time has passed, and the direction key is still held,
; then we  the autoshift.

; * t-last-autoshift is the time in ms when autoshift was last done.
; this is needed to know when to autoshift the key on a tick

; * t-last-drop is the time in ms when the piece was last dropped,
; including soft drop and fast drop


;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

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


; Tetris -> Tetris
; Return the Piece after it was moved down as much as possible
(define (tetris-drop-to-ground t)
    (define piece (tetris-piece t))
  (define plf (tetris-playfield t))
  (let* ([new-piece (try-move-piece 'down piece plf)]
         [same (equal? piece new-piece)])
    (if same t
        (tetris-drop-to-ground
         (struct-copy tetris t [piece new-piece])))))


; Piece, Playfeld -> GhostY
(define (tetris-calc-ghostY t)
  (posn-y (piece-posn (tetris-piece (tetris-drop-to-ground t)))))


; Tetris -> Piece
(define (tetris-ghost-piece t)
  (let* ([p (tetris-piece t)]
         [ghostY  (tetris-calc-ghostY t)]
         [pieceX (posn-x (piece-posn p))]
         [ghost-posn (make-posn pieceX ghostY)])
    (struct-copy piece p
                 [posn ghost-posn])))


; Tetris -> List of Blocks
(define (tetris-ghost-blocks t)
  (map (lambda (b) (struct-copy block b [color 'ghost]))
       (piece-blocks (tetris-ghost-piece t))))


; Tetris, Key -> Tetris
; Call each time a direction key has been just pressed,
; update the t-start-dirn state variable
(define (tetris-update-t-start-dirn t k ms)
  (if (not (or (key=? k "right")
               (key=? k "left")))
      t
      (let* ([dirn-pressed (or (tetris-key-pressed? t "left")
                               (tetris-key-pressed? t "right"))])
        (if dirn-pressed
            t
            (struct-copy tetris t [t-start-dirn ms])))))


; Tetris Key -> Tetris
; Update the last-direction-right? state variable
(define (tetris-update-last-dirn t k)
  (if (not (or (key=? k "right")
               (key=? k "left")))
      t
      (struct-copy tetris t
                   [last-dirn-right? (key=? k "right")])))


; Tetris Key -> Boolean
(define (tetris-key-pressed? t k)
  (hash-ref (tetris-h-key-state t) k #f))


; Tetris, Key, Boolean -> Tetris
; Set whether a key is pressed or released
(define (tetris-update-key-state t k pressed)
  (let* ([h (tetris-h-key-state t)] 
         [new-h (hash-set h k pressed)])
    (struct-copy tetris t [h-key-state new-h])))


;; Examples:
(define playfield0 `())
(define piece0 (make-piece (make-posn 5 (- HEIGHT 3)) 'L 0))
(define tetris0
  (make-tetris piece0 playfield0 -1
               (make-immutable-hash)
               #t 0 0 0))
(define playfield1 `(,(make-block (make-posn 0 0) 'ghost)))


(define (spawn-piece)
  (make-piece (make-posn 4 (- HEIGHT 3))
              (random-choice shape-names)
              0))


; Matrix of Booleans -> List of Pairs of Integer
; The pairs represent the offset coordinates to apply to the Piece position
; with the origin at the bottom-left of the Shape
(define (shape-to-pos-offsets m)
  (let* ([len (length m)]
         [block-at?
          (λ (posn)
            (let* ([x (car posn)]
                   [y (second posn)]
                   [h (length m)])
              (list-ref (list-ref m (- h y 1)) x)))])
    (filter block-at? (cartesian-product (build-list len identity)
                                         (build-list len identity)))))
(check-equal?  (andmap (λ (p) (member? p '((0 1) (1 1) (2 1) (2 2))))
                       (shape-to-pos-offsets (hash-ref h-shapes-rot '(L 0))))
               #t)


; Piece -> List of Blocks
; Returns a list of blocks that the piece consists of
(define (piece-blocks piece)
  (let* ([sh-name (piece-shape-name piece)]
         [sh (hash-ref h-shapes-rot
                       `(,sh-name
                         ,(piece-rotation piece)))]
         [coords (shape-to-pos-offsets sh)])
    (map (λ (coord)
           (make-block
            (make-posn (+ (first coord) (posn-x (piece-posn piece)))
                       (+ (second coord) (posn-y (piece-posn piece))))
            sh-name))
         coords)))

(check-true
 (let* ([res-ls (map block-posn (piece-blocks (make-piece (make-posn 0 0) 'L 0)))]
        [expected-ls (map block-posn `(,(make-block (make-posn 0 1) 'ghost)
                                       ,(make-block (make-posn 1 1) 'ghost)
                                       ,(make-block (make-posn 2 1) 'ghost)
                                       ,(make-block (make-posn 2 2) 'ghost)))])
   (andmap (λ (e) (member? e expected-ls)) res-ls)))
(check-true
 (let* ([res-ls (map block-posn (piece-blocks (make-piece (make-posn 1 5) 'J 0)))]
        [expected-ls (map block-posn `(,(make-block (make-posn 1 6) 'ghost)
                                       ,(make-block (make-posn 2 6) 'ghost)
                                       ,(make-block (make-posn 3 6) 'ghost)
                                       ,(make-block (make-posn 1 7) 'ghost)))])
   (andmap (λ (e) (member? e expected-ls)) res-ls)))


; Tetris -> List of Blocks
; It's convenient to treat a Tetris as just a list of blocks for drawing it
(define (tetris-blocks t)
  `(,@(tetris-ghost-blocks t)
    ,@(piece-blocks (tetris-piece t))
    ,@(tetris-playfield t)))


; Any -> Boolean
(define (playfield? plf)
  (if (list? plf)
      (andmap (λ (el) (block? el)) plf)
      #f))


; Playfield -> List of Blocks
; Just in case the representation changes
(define (playfield-blocks plf)
  plf)


; [Playfield / Piece / Tetris] -> List of Blocks
; Convert anything to blocks, or return an error
(define (any-to-blocks e)
  (cond
    [(block? e) e]
    [(playfield? e) (playfield-blocks e)]
    [(piece? e) (piece-blocks e)]
    [(tetris? e) (tetris-blocks e)]
    [else (error "Argument can't be converted to blocks.")]))


; List of [Block / Piece / Tetris] or just any of these -> List of Blocks
(define (convert-to-blocks ls)
  (flatten
   (if (list? ls)
       (map (λ (e) (any-to-blocks e)) ls)
       (any-to-blocks ls))))


; Block Playfield -> Boolean
; Return true if block is overlapping with another block
; or if it's outside the playfield
(define (block-overlapping-playfield? b plf)
  (cond
    [(empty? plf) #f]
    [else (or (equal? (block-posn b) (block-posn (first plf)))
              (block-overlapping-playfield? b (rest plf)))]))
(check-equal?
 (block-overlapping-playfield? (make-block (make-posn 0 1) 'ghost) (list (make-block (make-posn 0 0) 'ghost)))
 #f)
(check-equal?
 (block-overlapping-playfield? (make-block (make-posn 0 0) 'ghost) (list (make-block (make-posn 0 0) 'ghost)))
 #t)


; Block Playfield -> Boolean
; Return true if block is outside the Playfield
(define (block-outside-playfield? b)
  (or (< (posn-x (block-posn b)) 0)
      (>= (posn-x (block-posn b)) WIDTH)
      (< (posn-y (block-posn b)) 0)
      (>= (posn-y (block-posn b)) HEIGHT)))
(check-equal? (block-outside-playfield? (make-block (make-posn -1 0) 'ghost)) #t)
(check-equal? (block-outside-playfield? (make-block (make-posn WIDTH 0) 'ghost)) #t)
(check-equal? (block-outside-playfield? (make-block (make-posn 0 HEIGHT) 'ghost)) #t)
(check-equal? (block-outside-playfield? (make-block (make-posn 0 -1) 'ghost)) #t)
(check-equal? (block-outside-playfield? (make-block (make-posn 0 0) 'ghost)) #f)


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


; RotateDirection, Piece, Playfield -> Piece
; RotateDirection is one of ['cw 'ccw]
; Try to rotate the piece according to the Super Rotation System,
; return it if succeeded, otherwise return the original
(define (try-rotate-piece dirn piece0 plf)
  (let* ([shape-name (piece-shape-name piece0)]
         [rot0 (piece-rotation piece0)]
         [new-rot (rotation+ rot0 dirn)]
         [kick-table (hash-ref h-kick-data shape-name)]
         [kick-list (hash-ref kick-table `(,rot0 . ,new-rot))]
         [try-kick  ; return #t if this kick doesn't fail, else #f
          (λ (k)
            (let* ([kicked-piece (piece-move piece0 k)]
                   [new-piece (piece-rotate kicked-piece dirn)]
                   [overlapping (piece-overlapping? new-piece plf)])
              (not overlapping)))]
         [kick-search-result (memf try-kick kick-list)])
    (if (list? kick-search-result)
        (let* ([posn0 (piece-posn piece0)]
               [kick (car kick-search-result)]
               [delta-posn (make-posn (car kick) (cadr kick))]
               [new-posn (posn+ posn0 delta-posn)])
          (struct-copy piece piece0
                       [posn new-posn]
                       [rotation new-rot]))
        piece0)))

; Test
[define p0 (make-piece (make-posn 0 2) 'J 0)]
[define p1 (make-piece (make-posn 1 0) 'J 3)]
[define plf (list (make-block (make-posn 0 0) 'ghost)
                  (make-block (make-posn 0 2) 'ghost)
                  (make-block (make-posn 2 4) 'ghost))]
(check-equal? (try-rotate-piece 'ccw p0 plf) p1)

[define p2 (make-piece (make-posn 0 2) 'J 0)]
[define p3 (make-piece (make-posn -1 3) 'J 1)]
[define plf2 (list (make-block (make-posn 0 0) 'ghost)
            (make-block (make-posn 0 2) 'ghost)
            (make-block (make-posn 2 4) 'ghost))]
(define rotate-test
  (beside (draw-any-blocks (struct-copy tetris tetris0 [piece p2] [playfield plf2]))
          (draw-any-blocks (struct-copy tetris tetris0 [piece p3] [playfield plf2]))))
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


(define plf-full1 `(,@(build-list 10 (λ (x) (make-block (make-posn x 0) 'ghost)))
                 ,(make-block (make-posn 5 1) 'ghost)
                 ,@(build-list 10 (λ (x) (make-block (make-posn x 2) 'ghost)))
                 ,(make-block (make-posn 4 3) 'L)))

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
(define plf-cleared1 `(,(make-block (make-posn 5 0) 'ghost)
                       ,(make-block (make-posn 4 1) 'L)))
(check-equal? (clear-lines plf-full1) plf-cleared1)
; See this diagram for a visual explanation
(define clear-lines-explanation
  (beside (draw-any-blocks plf-full1)
          (text " Clear lines -> " 25 'black)
          (draw-any-blocks plf-cleared1)))


; Piece, Playfield -> Playfield
(define (add-piece-to-playfield p plf)
  (de-nest (list (piece-blocks p) plf)))


; Tetris -> Tetris
; Lock the piece at its current position and spawn a new one,
; updating the ghostY at the same time
(define (tetris-lock-and-spawn t)
  (let* ([p (tetris-piece t)]
         [plf (tetris-playfield t)]
         [plf1 (add-piece-to-playfield p plf)]
         [plf2 (clear-lines plf1)] 
         [new-piece (spawn-piece)]
         [new-ghostY (tetris-calc-ghostY t)])
    (struct-copy tetris t
                 [playfield plf2]
                 [piece new-piece]
                 [ghostY new-ghostY])))


; Tetris -> Tetris
; Drop active Piece due to gravity,
; or if it landed, lock it, clear the lines, and spawn a new Piece
(define (try-drop-piece-and-lock t)
  (let* ([piece (tetris-piece t)]
         [plf (tetris-playfield t)]
         [new-piece (try-move-piece 'down piece plf)]
         [landed (equal? (piece-posn piece) (piece-posn new-piece))])
    (if landed
        (tetris-lock-and-spawn t)
        (struct-copy tetris t [piece new-piece]))))


; Tetris -> Tetris
; after a tick, check if it's time to drop the piece 
(define (tetris-tick-drop t time)
  (define t-last-drop (tetris-t-last-drop t))
  (if (< (- time t-last-drop)
         T-DROP-RATE)
      t
      (let* ([new-t-last-drop
              (+ t-last-drop
                 T-DROP-RATE)]
             [t1 (try-drop-piece-and-lock t)]
             [t2 (struct-copy tetris t1
                              [t-last-drop new-t-last-drop])])
        t2)))


; Tetris -> Tetris
; Autoshift and update the timer
(define (tetris-autoshift t)
  (let* ([t-start-dirn (tetris-t-start-dirn t)]
         [t-last-autoshift (tetris-t-last-autoshift t)]
         [new-t-last-autoshift
          (if (< t-last-autoshift t-start-dirn)  ; autoshift just started
              (+ t-start-dirn T-DELAY-AUTOSHIFT)
              (+ t-last-autoshift T-AUTOSHIFT-RATE))]
         [t1 (struct-copy tetris t
                          [t-last-autoshift new-t-last-autoshift])])
    (cond
      [(and (tetris-key-pressed? t "left")
            (or (not (tetris-last-dirn-right? t))
                (not (tetris-key-pressed? t "right"))))
       (tetris-move t1 'left)]
      [(and (tetris-key-pressed? t "right")
            (or (tetris-last-dirn-right? t)
                (not (tetris-key-pressed? t "left"))))
       (tetris-move t1 'right)]
      [else t1])))


; Tetris -> Tetris
; Start autoshifting if enough time has passed
; since the last direction key press
; and since the last autoshift
(define (tetris-tick-autoshift t time)
  [define t-start-dirn (tetris-t-start-dirn t)]
  [define t-last-autoshift (tetris-t-last-autoshift t)]
  (if (or (< (- time t-start-dirn)
             T-DELAY-AUTOSHIFT)
          (< (- time t-last-autoshift)
             T-AUTOSHIFT-RATE))
      t
      (tetris-autoshift t)))


; Tetris, Move -> Tetris
; Move is one of '(left right cw ccw 180)
(define (tetris-move t mov)
  (let* ([p (tetris-piece t)]
         [plf (tetris-playfield t)]
         [new-piece
          (cond
            [(or (eq? mov 'left) (eq? mov 'right)) (try-move-piece mov p plf)]
            [(or (eq? mov 'cw) (eq? mov 'ccw)) (try-rotate-piece mov p plf)]
            [(eq? mov 180) (try-rotate-piece-180 p plf)]
            [else (error "Invalid argument for move.")])]
         [new-ghostY (tetris-calc-ghostY t)])
    (struct-copy tetris t
                 [piece new-piece]
                 [ghostY new-ghostY])))


; Piece Playfield -> Piece
(define (tetris-soft-drop t)
  (tetris-lock-and-spawn (tetris-drop-to-ground t)))


; Tetris Key -> Tetris
; called when a key is just pressed
(define (tetris-key-just-pressed t k ms)
  (cond
    [(key=? k "left") (tetris-move t 'left)]
    [(key=? k "right") (tetris-move t 'right)]
    [(key=? k " ") (tetris-soft-drop t)]
    [(or (key=? k "up") (key=? k "x")) (tetris-move t 'cw)]
    [(key=? k "z") (tetris-move t 'ccw)]
    [(key=? k "a") (tetris-move t 180)]
    [else t]))
