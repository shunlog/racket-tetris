#lang racket/base

;; This module implements a "Frozen" Tetris.
;; Think of it as a Tetris that's been frozen in time:
;; all the inputs that have an immediate effect still work,
;; whereas everything that happens with a delay doesn't.
;;
;; So pretty much everything is handled:
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


;; All of the movement functions raise an exception if the piece can't be moved.
;; For example `frozen-tetris-drop` will raise an exception when the piece is already on the floor.
;; The lock and spawn functions raise exceptions when it's game-over.
;;
;; This is useful for the Tetris "driver" that wraps the FrozenTetris
;; which will handle the rest of the logic.
;; This way, the Tetris driver can immediately know that it's time to lock the piece.
;; This is a better solution than the two alternatives:
;;   1. return #f on failure
;;   2. return an unmodified FrozenTetris
;; In the 1st case, we could forget to check for the false value, and it would propagate further
;; In the 2nd case, we would have to double check if the piece has moved, which is redundant


(require racket/contract)
(provide
 (contract-out
  [new-frozen-tetris (->* ()
                          ((or/c shape-name? false/c)
                           #:rows natural-number/c
                           #:cols natural-number/c)
                          frozen-tetris?)]
  [frozen-tetris? (-> any/c boolean?)]
  [frozen-tetris-playfield (-> frozen-tetris? playfield?)]
  [frozen-tetris-drop (-> frozen-tetris? frozen-tetris?)]
  [frozen-tetris-hard-drop (-> frozen-tetris? frozen-tetris?)]
  [frozen-tetris-right (-> frozen-tetris? frozen-tetris?)]
  [frozen-tetris-left (-> frozen-tetris? frozen-tetris?)]
  [frozen-tetris-lock (-> frozen-tetris? frozen-tetris?)]
  [frozen-tetris-spawn (-> frozen-tetris? shape-name? frozen-tetris?)]))


; -------------------------------
; Requires


(require racket/set)
(require lang/posn)
(require threading)
(require "block.rkt")
(require "playfield.rkt")
(require "shapes.rkt")
(require "utils.rkt")


;; ----------------------------
;; Definitions


; A FrozenTetris is a struct:
; - piece: Piece
; - locked: Playfield
(struct frozen-tetris [piece locked])


; A Piece is a struct:
; - posn: Posn
; - shape-name: one of shape-names

;; Think of the Piece as a "blueprint" instead of actual blocks,
;; visualize it as a lump of faded blocks that you can move around.
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
(define (frozen-tetris-spawn ft shape-name)
  (define grid-cols
    (~> ft frozen-tetris-locked playfield-cols))
  (define grid-rows
    (~> ft frozen-tetris-locked playfield-rows))
  (define piece-x
    (floor (/ (- grid-cols (shape-width shape-name)) 2)))

  ;; the lowest blocks should spawn on the first line of the vanish zone,
  ;; so if there are 20 rows, it should spawn on row with index 20 (0-based)
  (define piece-y
    (- grid-rows (shape-gap-below shape-name)))
  
  (define new-piece
    (piece (make-posn piece-x piece-y) shape-name 0))
  
  (cond
    [(not (playfield-can-place? (frozen-tetris-locked ft)
                                (piece-blocks new-piece)))
     (error "Can't spawn piece (block out)")]
    [else (struct-copy frozen-tetris ft
                       [piece new-piece])]))


(module+ test
  (test-case
      "Spawn a piece"
    (define ft0
      (frozen-tetris (piece (make-posn 0 0) 'L 0)
                     (empty-playfield 10 2)))
    ;; Spawn an L on the left
    (check
     block-lists=?
     (~> ft0
         (frozen-tetris-spawn 'L)
         frozen-tetris-playfield
         playfield-blocks)
     (strings->blocks '(".....L...."
                        "...LLL...."
                        ".........."
                        "..........")))

    ;; Spawn a I in the middle
    (check
     block-lists=?
     (~> ft0
         (frozen-tetris-spawn 'I)
         frozen-tetris-playfield
         playfield-blocks)
     (strings->blocks '("...IIII..."
                        ".........."
                        ".........."))))
  
  (test-case
      "Block out: the spawned piece overlaps with locked blocks."
    
    (define ft-full
      (struct-copy
       frozen-tetris
       (new-frozen-tetris 'O)
       [locked (playfield-add-blocks (empty-playfield 10 20)
                                     (list (block 5 21 'I)))]))
    (check-exn
     #rx"block out"
     (λ () (frozen-tetris-spawn ft-full 'L))))
  )


(define (new-frozen-tetris [shape-name #f]
                           #:cols [cols 10]
                           #:rows [rows 20])
  (define new-ft (frozen-tetris #f (empty-playfield cols rows)))
  (if (not shape-name)
      new-ft
      (frozen-tetris-spawn new-ft shape-name)))


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


; Add a posn to piece position
(define (piece-move p posn)
  (struct-copy piece p
               [posn (posn+ posn (piece-posn p))]))


; FrozenTetris -> Playfield
(define (frozen-tetris-playfield ft)
  (define locked (frozen-tetris-locked ft))
  (define piece (frozen-tetris-piece ft))
  (cond
    [(not piece) (error "Piece is #f")]
    [else (playfield-add-blocks
           locked
           (piece-blocks piece))]))


(module+ test
  (test-case
      "Get all blocks from a FrozenTetris"

    ;; A Piece and some locked blocks
    (define plf1
      (playfield-add-blocks
       (empty-playfield)
       (strings->blocks '("JJ."))))
    (define ft1
      (frozen-tetris
       (piece (make-posn 0 0) 'T 0)
       plf1))
    (check
     block-lists=?
     (playfield-blocks (frozen-tetris-playfield ft1))
     (playfield-blocks (playfield-add-blocks
                        plf1
                        (strings->blocks '(".T."
                                           "TTT"
                                           "..."))))))
  (test-case
      "Error if null Piece"
    (check-exn
     exn:fail?
     (λ () (frozen-tetris-playfield (frozen-tetris #f (empty-playfield 3 3)))))))


; FrozenTetris Posn -> FrozenTetris
(define (frozen-tetris-move ft posn)
  (define new-piece (piece-move (frozen-tetris-piece ft) posn))
  (define locked (frozen-tetris-locked ft))
  (cond
    [(playfield-can-place? locked (piece-blocks new-piece))
     (struct-copy frozen-tetris ft
                  [piece new-piece])]
    [else (error "Can't move piece by " posn)]))


; FrozenTetris -> FrozenTetris
(module+ test
  (test-case
      "Move the frozen-tetris piece"
    (define ft0
      (frozen-tetris
       (piece (make-posn 0 0) 'I 0)
       (~> (empty-playfield 4 3)
           (playfield-add-blocks (strings->blocks '("JJ.."))))))

    ;; Successful move
    (define ft0-drop
      (frozen-tetris-move ft0 (make-posn 0 -1)))
    (check
     block-lists=?
     (playfield-blocks (frozen-tetris-playfield ft0-drop))
     (strings->blocks '("IIII"
                        "JJ..")))

    ;; Fail on collision with locked blocks
    (check-exn
     exn:fail?
     (λ () (frozen-tetris-move ft0-drop (make-posn 0 -1))))

    ;; Fail on collision with bottom of playfield
    (define ft1
      (frozen-tetris
       (piece (make-posn 0 -1) 'L 0)
       (empty-playfield 3 3)))    
    (check-exn
     exn:fail?
     (λ () (frozen-tetris-move ft1 (make-posn 0 -1))))

    ;; Fail on collision with right border    
    (check-exn
     exn:fail?
     (λ () (frozen-tetris-move ft1 (make-posn 1 0))))

    ;; Fail on collision with left border    
    (check-exn
     exn:fail?
     (λ () (frozen-tetris-move ft1 (make-posn -1 0))))
    
    ;; Should work on new instance
    (check-not-exn
     (λ () (frozen-tetris-move (new-frozen-tetris 'L) (make-posn 0 -1))))
    ))


(define (frozen-tetris-drop ft)
  (frozen-tetris-move ft (make-posn 0 -1)))

(define (frozen-tetris-right ft)
  (frozen-tetris-move ft (make-posn 1 0)))

(define (frozen-tetris-left ft)
  (frozen-tetris-move ft (make-posn -1 0)))


;; Move the Piece as far down as possible
(define (frozen-tetris-hard-drop ft)
  (define dropped
    (with-handlers ([exn:fail? (λ (e) #f)])
      (frozen-tetris-drop ft)))
  (if (not dropped)
      ft
      (frozen-tetris-hard-drop dropped)))


(module+ test
  (test-case
      "Hard drop to the floor"
    (define ft0 (new-frozen-tetris 'O #:cols 4 #:rows 2))
    ;; assert initial setup
    (check
     block-lists=?
     (playfield-blocks (frozen-tetris-playfield ft0))
     (strings->blocks '(".OO."
                        ".OO."
                        "...."
                        "....")))

    (define ft-dropped (frozen-tetris-hard-drop ft0))
    (check
     block-lists=?
     (playfield-blocks (frozen-tetris-playfield ft-dropped))
     (strings->blocks '("...."
                        "...."
                        ".OO."
                        ".OO.")))
    )

  (test-case
      "Hard drop to a block"
    (define ft0 (new-frozen-tetris 'O #:cols 4 #:rows 3))
    (define plf1 (playfield-add-blocks (frozen-tetris-locked ft0)
                                       (strings->blocks '("LL.."))))
    (define ft1 (struct-copy frozen-tetris ft0 [locked plf1]))
    
    ;; assert initial setup
    (check
     block-lists=?
     (playfield-blocks (frozen-tetris-playfield ft1))
     (strings->blocks '(".OO."
                        ".OO."
                        "...."
                        "...."
                        "LL..")))

    (define ft-dropped (frozen-tetris-hard-drop ft1))
    (check
     block-lists=?
     (playfield-blocks (frozen-tetris-playfield ft-dropped))
     (strings->blocks '("...."
                        "...."
                        ".OO."
                        ".OO."
                        "LL..")))
    )
  )


; FrozenTetris -> FrozenTetris
; Locks piece (adds the blocks to `locked`) and clears the blueprint (sets Piece to #f)
; Raises error on lock out.
(define (frozen-tetris-lock ft)
  (define piece (frozen-tetris-piece ft))
  (define plf-rows (playfield-rows (frozen-tetris-locked ft)))
  (define piece-min-y
    (+ (posn-y (piece-posn piece))
       (shape-gap-below (piece-shape-name piece))))
  (cond
    [(>= piece-min-y plf-rows)
     (error "All piece blocks above ceiling: lock out")]
    [else
     (define new-locked
       (~> ft
           frozen-tetris-piece
           piece-blocks
           (playfield-add-blocks (frozen-tetris-locked ft) _)))
     (struct-copy frozen-tetris ft
                  [locked new-locked]
                  [piece #f])]))


(module+ test
  (test-case
      "Lock out if locked right after spawn (since pieces spawn above the ceiling)"    
    (define ft0 (new-frozen-tetris 'L))
    (check-exn
     #rx"lock out"
     (λ () (frozen-tetris-lock ft0))))

  
  (test-case
      "No lock out if at least a single block below ceiling"
    (define ft-dropped
      (~> (new-frozen-tetris 'O)
          frozen-tetris-drop
          ; move it out of the way
          frozen-tetris-right
          frozen-tetris-right
          frozen-tetris-right))
    (check-not-exn
     (λ () (frozen-tetris-lock ft-dropped)))))
