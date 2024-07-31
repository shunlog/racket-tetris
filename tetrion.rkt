#lang racket/base

;; This module implements a "frozen" Tetris, or a "Tetris machine".
;; Think of it as a Tetris that's been frozen in time:
;; all the inputs that have an immediate effect still work,
;; whereas everything that happens with a delay doesn't.
;;
;; You can all the basic actions:
;;   - pressing Left/Right moves the piece
;;   - pressing Space drops it
;;   - you can move the piece down with a function call
;;   - you can lock the piece with a function call
;;   - piece spawn, score update, game over check
;;
;; Except for actions that need information about time:
;;   - autoshift
;;   - gravity
;;   - lock delay


;; One quirk is that actions are handled a bit more atomically.
;; For example, while you might expect that locking a piece would clear the lines,
;; for the Tetrion these are separate actions.
;; All of these are separate:
;;   - locking the piece
;;   - spawning a piece
;;   - clearing lines (together with updating the score)

;; All of the movement functions raise an exception if the piece can't be moved.
;; For example `tetrion-drop` will raise an exception when the piece is already on the floor.
;; The lock and spawn functions raise exceptions when it's game-over.
;;
;; This is useful for the Tetris "driver" that wraps the Tetrion
;; which will handle the rest of the logic.
;; This way, the Tetris driver can immediately know that it's time to lock the piece.
;; This is a better solution than the two alternatives:
;;   1. return #f on failure
;;   2. return an unmodified Tetrion
;; In the 1st case, we could forget to check for the false value, and it would propagate further
;; In the 2nd case, we would have to double check if the piece has moved, which is redundant


(require racket/contract)
(provide
 (contract-out
  [new-tetrion (->* ()
                    (#:starting-shape (or/c shape-name? boolean?)
                     #:rows natural-number/c
                     #:cols natural-number/c
                     #:shape-generator shape-generator?
                     #:locked-blocks (listof block?))
                    tetrion?)]
  [tetrion? (-> any/c boolean?)]

  ;; Accessors
  [tetrion-playfield (-> tetrion? playfield?)]

  ;; Movement
  [tetrion-drop (-> tetrion? tetrion?)]
  [tetrion-hard-drop (-> tetrion? tetrion?)]
  [tetrion-right (-> tetrion? tetrion?)]
  [tetrion-left (-> tetrion? tetrion?)]
  [tetrion-rotate (-> tetrion? boolean? tetrion?)]
  [tetrion-rotate-180 (-> tetrion? tetrion?)]

  ;; Other
  [tetrion-lock (-> tetrion? tetrion?)]
  [tetrion-spawn (->* (tetrion?)
                      (shape-name?)
                      tetrion?)]))


; -------------------------------
; Requires


(require lang/posn)
(require threading)
(require "block.rkt")
(require "playfield.rkt")
(require "shapes.rkt")
(require "utils.rkt")


;; ----------------------------
;; Definitions


; A Tetrion is a struct:
; - piece: Piece
; - locked: Playfield
(struct tetrion [piece locked shape-generator])


; A Piece is a struct:
; - posn: Posn
; - shape-name: shape-name?

;; Think of the Piece as a "blueprint" instead of actual blocks,
;; visualize it as a lump of faded blocks that you can move around.
;; When we draw the Tetrion, we convert the Piece to its constituent blocks,
;; but internally, it is just a blueprint that we can move around and even set to #f.
;;
;; Some consequences:
;; 1. Locking means adding the blueprint blocks to the `locked` Playfield.
;;   The blueprint doesn't change, it's still there in the same place,
;;   overlapping with the blocks you just added.
;; 2. Spawning a new piece then simply means creating a new blueprint.

;; After locking, we want to clear the "blueprint",
;; otherwise its blocks would overlap with the locked blocks,
;; which isn't a valid state.
;; That means setting the Piece to #f should be a valid state.
;; We should keep in mind this possibility whenever we try to move/rotate the piece.
(struct piece [posn shape-name rotation])

; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


;; Spawns a given shape,
;; meaning changes the piece "blueprint"
(define (tetrion-spawn
         tn 
         [shape-name ((tetrion-shape-generator tn))]
         #:x [x #f]
         #:y [y #f]
         #:rotation [rotation 0])
  (define grid-cols
    (~> tn tetrion-locked playfield-cols))
  (define grid-rows
    (~> tn tetrion-locked playfield-rows))
  (define piece-x
    (or x
        (floor (/ (- grid-cols (shape-width shape-name)) 2))))

  ;; the lowest blocks should spawn on the first line of the vanish zone,
  ;; so if there are 20 rows, it should spawn on row with index 20 (0-based)
  (define piece-y
    (or y
        (- grid-rows (shape-gap-below shape-name))))
  
  (define new-piece
    (piece (make-posn piece-x piece-y) shape-name rotation))
  
  (cond
    [(not (playfield-can-place? (tetrion-locked tn)
                                (piece-blocks new-piece)))
     (error "Can't spawn piece (block out)")]
    [else (struct-copy tetrion tn
                       [piece new-piece])]))


(module+ test
  (test-case
      "Spawn a piece"
    (define ft0
      (new-tetrion #:starting-shape 'L
                         #:cols 10 #:rows 2))
    ;; Spawn an L on the left
    (check
     block-lists=?
     (~> ft0
         (tetrion-spawn 'L)
         tetrion-playfield
         playfield-blocks)
     (strings->blocks '(".....L...."
                        "...LLL...."
                        ".........."
                        "..........")))

    ;; Spawn a I in the middle
    (check
     block-lists=?
     (~> ft0
         (tetrion-spawn 'I)
         tetrion-playfield
         playfield-blocks)
     (strings->blocks '("...IIII..."
                        ".........."
                        ".........."))))

  (test-case
      "Spawn piece at specific position"
    (define ft0
      (~> (new-tetrion #:cols 4 #:rows 10)
          (tetrion-spawn 'T #:x 0 #:y 0 #:rotation 3)))

    (check
     block-lists=?
     (~> ft0 tetrion-playfield playfield-blocks)
     (strings->blocks '(".T.."
                        "TT.."
                        ".T..")))    
    )
  
  (test-case
      "Block out: the spawned piece overlaps with locked blocks."
    
    (define tn-full
      (struct-copy
       tetrion
       (new-tetrion #:starting-shape 'O)
       [locked (playfield-add-blocks (empty-playfield 10 20)
                                     (list (block 5 21 'I #f)))]))
    (check-exn
     #rx"block out"
     (λ () (tetrion-spawn tn-full 'L))))
  )


;; Starting-shape can be either a shape-name,
;; #f for null shape,
;; or #t for generating a shape
(define (new-tetrion
         #:shape-generator [shape-generator 7-loop-shape-generator]
         #:starting-shape [start-shape (shape-generator)]
         #:cols [cols 10]
         #:rows [rows 20]
         #:locked-blocks [locked-blocks '()])

  (define new-tn
    (tetrion #f
                   (playfield-add-blocks (empty-playfield cols rows) locked-blocks)
                   shape-generator))
  (cond
    [(not start-shape) new-tn]
    [else (tetrion-spawn new-tn start-shape)]))


;; Return the blocks representing a Piece
;; by adding the piece's position to each block representing its shape
(define (piece-blocks piece)
  (define sname (piece-shape-name piece))
  (define rot (piece-rotation piece))
  (define blocks-at-origin (shape-name->blocks sname rot))
  (for/list ([blck blocks-at-origin])
    (block-move blck (piece-posn piece))))


; Add two Posn's
(define (posn+ p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))


; Tetrion -> Playfield
(define (tetrion-playfield tn)
  (define piece (tetrion-piece tn))
  (playfield-add-blocks
   (tetrion-locked tn)
   (if (not piece) '() (piece-blocks piece))))


(module+ test
  (test-case
      "Get playfield immediately after lock, with null Piece"

    (define ft1 (~> (new-tetrion #:starting-shape 'T #:cols 3 #:rows 2)
                    tetrion-hard-drop
                    tetrion-lock))
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield ft1))
     (strings->blocks '(".T."
                        "TTT")))))


; Tetrion Posn -> Tetrion
(define (tetrion-move tn posn)
  (define p (tetrion-piece tn))
  (define moved-piece (struct-copy piece p
                 [posn (posn+ posn (piece-posn p))]))
  (define locked (tetrion-locked tn))
  (cond
    [(playfield-can-place? locked (piece-blocks moved-piece))
     (struct-copy tetrion tn
                  [piece moved-piece])]
    [else (error "Can't move piece by " posn)]))


;; Rotate cw or ccw,
;; Raise exn:fail if can't rotate
(define (tetrion-rotate tn cw?)
  (define p (tetrion-piece tn))
  (define shape-name (piece-shape-name p))
  (define rot-initial (piece-rotation p))
  (define rot-final (modulo (+ rot-initial (if cw? 1 -1)) 4))
  (define rotated-p (struct-copy piece p [rotation rot-final]))
  (define tn-rotated (struct-copy tetrion tn [piece rotated-p]))
  (define plf (tetrion-playfield tn))

  (define (moved-or-false kick)
    (with-handlers
      ([exn:fail? (λ (e) #f)])
      (tetrion-move tn-rotated (make-posn (car kick) (cadr kick)))))

  (define new-tn
    (for/first ([kick (kick-data shape-name rot-initial rot-final)]
                #:when (moved-or-false kick))
      (moved-or-false kick)))

  (cond
    [(not new-tn) (error "Can't rotate piece." tn)]
    [else new-tn]))

(module+ test
  (test-case
      "Rotate cw"
    (define ft0 (new-tetrion #:starting-shape 'L #:cols 4 #:rows 2))

    ;; assert initial setup
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield ft0))
     (strings->blocks '("..L."
                        "LLL."
                        "...."
                        "....")))

    (define ft1 (tetrion-rotate ft0 #t))
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield ft1))
     (strings->blocks '(".L.."
                        ".L.."
                        ".LL."
                        "....")))

    (define ft2 (~> ft1
                   tetrion-left
                   (tetrion-rotate #t)))
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield ft2))
     (strings->blocks '("...."
                        "LLL."
                        "L..."
                        "....")))
    )
)


;; Rotate 180, raise exn:fail if can't
(define (tetrion-rotate-180 tn)
  (define (try-rotate-cw ft1)
    (with-handlers ([exn:fail? (λ (e) #f)])
      (tetrion-rotate ft1 #t)))
  (define (try-change-rotation)
    (define p (tetrion-piece tn))
    (define new-rot (modulo (+ 2 (piece-rotation p)) 4))
    (define new-p (struct-copy piece p [rotation new-rot]))
    (if (playfield-can-place? (tetrion-locked tn) (piece-blocks new-p))
        (struct-copy tetrion tn [piece new-p])
        #f))
  (or
   (try-change-rotation)
   ;; try rotating twice clockwise
   (and~> tn try-rotate-cw try-rotate-cw)
   (error "Can't rotate 180.")))

(module+ test
  (test-case
      "Rotate 180 by simply changing the piece rotation."
    (define ft0
      (~> (new-tetrion
           #:cols 4 #:rows 10
           #:locked-blocks (strings->blocks '("OO."
                                              "..."
                                              ".OO")))
          (tetrion-spawn 'L #:x 0 #:y 0 #:rotation 0)))

    ;; assert initial setup
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield ft0))
     (strings->blocks '("OOL"
                        "LLL"
                        ".OO")))

    (define tn-rotated (tetrion-rotate-180 ft0))
    (check
     block-lists=?
     (~> tn-rotated tetrion-playfield playfield-blocks)
     (strings->blocks '("OO."
                        "LLL"
                        "LOO")))
    
    )
  )


; Tetrion -> Tetrion
(module+ test
  (test-case
      "Move the tetrion piece"
    (define ft0
      (new-tetrion #:starting-shape 'I
                         #:cols 4
                         #:rows 2
                         #:locked-blocks (strings->blocks '("JJ.."))))

    ;; check assumption
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield ft0))
     (strings->blocks '("IIII"
                        "...."
                        "JJ..")))
    
    ;; Successful move
    (define ft0-drop
      (tetrion-move ft0 (make-posn 0 -1)))
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield ft0-drop))
     (strings->blocks '("IIII"
                        "JJ..")))

    ;; Fail on collision with locked blocks
    (check-exn
     exn:fail?
     (λ () (tetrion-move ft0-drop (make-posn 0 -1))))

    ;; Fail on collision with bottom of playfield
    (define ft1
      (new-tetrion #:starting-shape 'L
                         #:cols 3
                         #:rows 0))    
    (check-exn
     exn:fail?
     (λ () (tetrion-move ft1 (make-posn 0 -1))))

    ;; Fail on collision with right border    
    (check-exn
     exn:fail?
     (λ () (tetrion-move ft1 (make-posn 1 0))))

    ;; Fail on collision with left border    
    (check-exn
     exn:fail?
     (λ () (tetrion-move ft1 (make-posn -1 0))))
    
    ;; Should work on new instance
    (check-not-exn
     (λ () (tetrion-move (new-tetrion #:starting-shape 'L) (make-posn 0 -1))))
    ))


(define (tetrion-drop tn)
  (tetrion-move tn (make-posn 0 -1)))

(define (tetrion-right tn)
  (tetrion-move tn (make-posn 1 0)))

(define (tetrion-left tn)
  (tetrion-move tn (make-posn -1 0)))


;; Move the Piece as far down as possible
(define (tetrion-hard-drop tn)
  (define dropped
    (with-handlers ([exn:fail? (λ (e) #f)])
      (tetrion-drop tn)))
  (if (not dropped)
      tn
      (tetrion-hard-drop dropped)))


(module+ test
  (test-case
      "Hard drop to the floor"
    (define ft0 (new-tetrion #:starting-shape 'O #:cols 4 #:rows 2))
    ;; assert initial setup
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield ft0))
     (strings->blocks '(".OO."
                        ".OO."
                        "...."
                        "....")))

    (define tn-dropped (tetrion-hard-drop ft0))
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield tn-dropped))
     (strings->blocks '("...."
                        "...."
                        ".OO."
                        ".OO.")))
    )

  (test-case
      "Hard drop to a block"
    (define ft0 (new-tetrion #:starting-shape 'O #:cols 4 #:rows 3))
    (define plf1 (playfield-add-blocks (tetrion-locked ft0)
                                       (strings->blocks '("LL.."))))
    (define ft1 (struct-copy tetrion ft0 [locked plf1]))
    
    ;; assert initial setup
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield ft1))
     (strings->blocks '(".OO."
                        ".OO."
                        "...."
                        "...."
                        "LL..")))

    (define tn-dropped (tetrion-hard-drop ft1))
    (check
     block-lists=?
     (playfield-blocks (tetrion-playfield tn-dropped))
     (strings->blocks '("...."
                        "...."
                        ".OO."
                        ".OO."
                        "LL..")))
    )
  )


; Tetrion -> Tetrion
; Locks piece (adds the blocks to `locked`) and clears the blueprint (sets Piece to #f)
; Raises error on lock out.
(define (tetrion-lock tn)
  (define piece (tetrion-piece tn))
  (define plf-rows (playfield-rows (tetrion-locked tn)))
  (define piece-min-y
    (+ (posn-y (piece-posn piece))
       (shape-gap-below (piece-shape-name piece))))
  (cond
    [(>= piece-min-y plf-rows)
     (error "All piece blocks above ceiling: lock out")]
    [else
     (define new-locked
       (~> tn
           tetrion-piece
           piece-blocks
           (playfield-add-blocks (tetrion-locked tn) _)))
     (struct-copy tetrion tn
                  [locked new-locked]
                  [piece #f])]))


(module+ test
  (test-case
      "Lock out if locked right after spawn (since pieces spawn above the ceiling)"    
    (define ft0 (new-tetrion #:starting-shape 'L))
    (check-exn
     #rx"lock out"
     (λ () (tetrion-lock ft0))))

  
  (test-case
      "No lock out if at least a single block below ceiling"
    (define tn-dropped
      (~> (new-tetrion #:starting-shape 'O)
          tetrion-drop
          ; move it out of the way
          tetrion-right
          tetrion-right
          tetrion-right))
    (check-not-exn
     (λ () (tetrion-lock tn-dropped)))))
