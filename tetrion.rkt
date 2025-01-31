#lang racket/base

;; A Tetrion is a "Tetris machine".
;; Think of it as mechanical Tetris that you control manually through commands:
;; move the piece left, drop the piece, lock the piece, spawn a new one.

;; For an actual Tetris, you are supposed to write a module that controls the Tetrion.
;; The benefit of having the Tetrion separate is modularity:
;; It is easier to ensure that every command issued on a Tetrion won't result in an invalid state
;; when you don't have to worry about timestamps and ticks.

;; Note:
;; Locking the piece and spawning a new one are two separate functions.
;; This enables us to test them separately.

;; All of the movement functions raise an exception if the piece can't be moved.
;; For example `tetrion-drop` will raise an exception when the piece is already on the floor.
;; The lock and spawn functions raise exceptions when it's game-over.

;; Note:
;; I think raising exceptions on failure is better than either:
;;   1. returning #f, or
;;   2. returning an unmodified Tetrion
;; In the 1st case, we could forget to check for the false value, and it would propagate further
;; In the 2nd case, we would have to double check if the piece has moved, which is redundant
;; TODO: use a custom exception type to distinguish from actual errors.


(require racket/contract)
(provide
 (contract-out
  [new-tetrion (->* ()
                    (#:rows natural-number/c
                     #:cols natural-number/c
                     #:shape-generator shape-generator?
                     #:locked-blocks (listof block?)
                     #:queue-size natural-number/c)
                    tetrion?)]
  [tetrion? (-> any/c boolean?)]

  ;; Accessors
  [tetrion-playfield (->* (tetrion?) (boolean?) playfield?)]
  [tetrion-queue (-> tetrion? (listof shape-name/c))]
  [tetrion-on-hold (-> tetrion? (or/c #f shape-name/c))]

  ;; Movement
  [tetrion-drop (-> tetrion? tetrion?)]
  [tetrion-hard-drop (-> tetrion? tetrion?)]
  [tetrion-right (-> tetrion? tetrion?)]
  [tetrion-left (-> tetrion? tetrion?)]
  [tetrion-rotate (-> tetrion? boolean? tetrion?)]
  [tetrion-rotate-180 (-> tetrion? tetrion?)]

  ;; Other
  [tetrion-lock (-> tetrion? tetrion?)]
  [tetrion-hold (-> tetrion? tetrion?)]
  [tetrion-spawn (-> tetrion? tetrion?)]
  [tetrion-spawn-shape (-> tetrion? shape-name/c tetrion?)]
  [tetrion-add-garbage (-> tetrion? natural-number/c tetrion?)]
  ))


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
; - locked: Playfield of the locked blocks
; - shape-generator: shape-generator? - a function with no arguments that returns a shape-name
; - queue: [list-of shape-name/c] - the list of next pieces, for preview. Must have at least 1 element.
; - on-hold: (or/c #f shape-name/c) - the shape on hold, or #f if none
; - can-hold: boolean - whether it is legal to put the current piece on hold.
;        by default, if the current piece was obtained from the hold, you can't put it back on hold.
(struct tetrion [piece locked shape-generator queue on-hold can-hold])


; A Piece is either #f or a struct:
; - posn: Posn
; - shape-name: shape-name/c

;; The Tetrion struct keeps the active piece blocks and the locked blocks separate.

;; Think of the Piece as a pair of (position, blueprint) instead of a list of blocks,
;; the blueprint being identified by a shape-name, provided by shapes.rkt.
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


;; Tetrion -> Tetrion
;; Spawn the shape popped from the queue and push the next generated one
(define (tetrion-spawn tn)
  (define next-shape (car (tetrion-queue tn)))
  (define new-shape ((tetrion-shape-generator tn)))
  (define new-queue (append (cdr (tetrion-queue tn)) (list new-shape)))
  (~> (tetrion-spawn-shape tn next-shape)
      (struct-copy tetrion _
                   [queue new-queue]
                   [can-hold #t])))


(module+ test
  (test-case
      "Spawn next from queue"
    (define tn0 (new-tetrion))
    (define next-shape (car (tetrion-queue tn0)))
    (define tn1 (tetrion-spawn tn0))
    (define spawned-shape (piece-shape-name (tetrion-piece tn1)))
    (check-equal? spawned-shape next-shape)
    ))


;; Tetrion -> Tetrion
;; If there was a piece on hold, swap it with the current piece,
;; otherwise put the current on hold, and spawn the next one from the queue
(define (tetrion-hold tn)
  (define current-sn (~> tn tetrion-piece piece-shape-name))
  (define sn-on-hold (~> tn tetrion-on-hold))
  (cond
    [(not (tetrion-can-hold tn)) (error "Can't hold piece.")]
    [(not sn-on-hold)                   ; no piece on hold yet
     (~> (tetrion-spawn tn)
         (struct-copy tetrion _ [on-hold current-sn] [can-hold #f]))]
    [else
     (~> (tetrion-spawn-shape tn sn-on-hold)
         (struct-copy tetrion _ [on-hold current-sn] [can-hold #f]))]))

(module+ test
  (test-case
      "Hold piece"
    (define tn0 (~> (new-tetrion) tetrion-spawn))
    ;; Hold is empty initially
    (check-equal? (tetrion-on-hold tn0) #f)

    ;; Put the piece on hold, next one should spawn
    (define shape0 (piece-shape-name (tetrion-piece tn0)))
    (define shape-q1 (car (tetrion-queue tn0)))
    (define tn1 (tetrion-hold tn0))
    (define spawned-shape (piece-shape-name (tetrion-piece tn1)))
    (check-equal? spawned-shape shape-q1)
    (check-equal? (tetrion-on-hold tn1) shape0)

    ;; Swap the piece with the one on hold
    (define tn2 (~> tn1 tetrion-spawn tetrion-spawn))
    (define tn3 (~> tn2 tetrion-hold))
    (check-equal? (piece-shape-name (tetrion-piece tn3))
                  (tetrion-on-hold tn2))
    (check-equal? (piece-shape-name (tetrion-piece tn2))
                  (tetrion-on-hold tn3)))

  (test-case
      "Can't hold twice in a row"
    (define tn0 (~> (new-tetrion) tetrion-spawn tetrion-hold))
    (check-exn exn:fail?
               (λ () (tetrion-hold tn0)))
    (check-not-exn
     (λ () (~> tn0 tetrion-spawn tetrion-hold)))))


;; Spawn a Piece according to the specified shape-name.
;; If the coordinates are not specified,
;; the piece will be centered at the bottom of the vanish zone.
(define (tetrion-spawn-shape tn
                             shape-name
                             #:x [x #f]
                             #:y [y #f]
                             #:rotation [rotation 0])
  (define cols (~> tn tetrion-locked playfield-cols))
  (define rows (~> tn tetrion-locked playfield-rows))

  (define shape-posns (shape-name->posns shape-name rotation))
  (define piece-width (add1 (- (apply max (map posn-x shape-posns))
                               (apply min (map posn-x shape-posns)))))
  (define leftmost-x (floor (/ (- cols piece-width) 2)))
  (define piece-x (or x
                      (- leftmost-x (apply min (map posn-x shape-posns)))))
  ;; the lowest blocks should spawn on the first line of the vanish zone,
  ;; so if there are 20 rows, it should spawn on row with index 20 (0-based)
  (define piece-y (or y
                      (- rows (apply min (map posn-y shape-posns)))))
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
      "Spawn a piece in the middle when symmetric"
    (define ft0
      (~> (new-tetrion #:cols 10 #:rows 2)
          (tetrion-spawn-shape 'L)))
    (check block-lists=?
           (~> ft0 (tetrion-spawn-shape 'L) tetrion-playfield playfield-blocks)
           (strings->blocks '(".....L...."
                              "...LLL...."
                              ".........."
                              ".........."))))

  (test-case
      "Spawn a piece on the left of the middle when asymmetric"
    (define ft1
      (~> (new-tetrion #:cols 10 #:rows 2)
          (tetrion-spawn-shape 'I)))
    (check block-lists=?
           (~> ft1 tetrion-playfield playfield-blocks)
           (strings->blocks '("...IIII..."
                              ".........."
                              ".........."))))

  (test-case
      "Spawn piece at a specific position and rotation"
    (define ft0
      (~> (new-tetrion #:cols 4 #:rows 10)
          (tetrion-spawn-shape 'T #:x 0 #:y 0 #:rotation 3)))
    (check block-lists=?
           (~> ft0 tetrion-playfield playfield-blocks)
           (strings->blocks '(".T.."
                              "TT.."
                              ".T.."))))

  (test-case
      "Spawn piece in vanish zone with specific rotation"
    (define ft0
      (~> (new-tetrion #:cols 4 #:rows 1)
          (tetrion-spawn-shape 'I #:rotation 1)))
    (check block-lists=?
           (~> ft0 tetrion-playfield playfield-blocks)
           (strings->blocks '(".I.."
                              ".I.."
                              ".I.."
                              ".I.."
                              "...."))))

  (test-case
      "Block out: the spawned piece overlaps with locked blocks."
    (define tn-full
      (new-tetrion #:locked-blocks (list (block (make-posn 5 21) (cons 'I 'normal)))))
    (check-exn
     #rx"block out"
     (λ () (tetrion-spawn-shape tn-full 'L))))
  )


(define (new-tetrion #:shape-generator [shape-generator 7-loop-shape-generator]
                     #:cols [cols 10]
                     #:rows [rows 20]
                     #:locked-blocks [locked-blocks '()]
                     #:queue-size [queue-size 5])
  (define locked (~> (empty-playfield cols rows)
                     (playfield-add-blocks locked-blocks)))
  (define queue (build-list queue-size (λ (_) (shape-generator))))
  (tetrion #f locked shape-generator queue #f #t))


;; Return the blocks representing a Piece
;; by adding the piece's position to each block representing its shape.
;; If ghost? is true, set the block type appropriately
(define (piece-blocks piece [ghost? #f])
  (cond
    [(not piece) '()]
    [else (define sname (piece-shape-name piece))
          (define rot (piece-rotation piece))
          (for/list ([pos (shape-name->posns sname rot)])
            (~> (block pos (cons sname (if ghost? 'ghost 'normal)))
                (block-move (piece-posn piece))))]))


; Tetrion -> Playfield
; Return the playfield with all the blocks combined: locked, piece and ghost (if specified)
(define (tetrion-playfield tn [ghost? #f])
  (define (add-piece-blocks plf)
    (playfield-add-blocks plf (piece-blocks (tetrion-piece tn))))
  (define (add-ghost-blocks plf)
    (define ghost-piece (~> (tetrion-hard-drop tn) tetrion-piece))
    (playfield-add-blocks-maybe plf (piece-blocks ghost-piece #t)))

  (define plf-locked (tetrion-locked tn))
  (define locked+piece (add-piece-blocks plf-locked))
  (if ghost?
      (add-ghost-blocks locked+piece)
      locked+piece))


(module+ test
  (define ft1 (~> (new-tetrion #:cols 4 #:rows 1)
                  (tetrion-spawn-shape 'T)))
  (define piece-blocks1
    (strings->blocks '(".T."
                       "TTT"
                       "...")))
  (define piece+ghost-blocks1
    (append piece-blocks1
            (list (block (make-posn 0 0) '(T . ghost))
                  (block (make-posn 1 0) '(T . ghost))
                  (block (make-posn 2 0) '(T . ghost)))))

  (test-case
      "Get Tetrion playfield without ghost piece blocks"

    (check block-lists=?
           (playfield-blocks (tetrion-playfield ft1))
           piece-blocks1))

  (test-case
      "Get Tetrion playfield with ghost piece blocks"
    (check block-lists=?
           (playfield-blocks (tetrion-playfield ft1 #t))
           piece+ghost-blocks1)))


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
    (define ft0 (~> (new-tetrion #:cols 4 #:rows 2)
                    (tetrion-spawn-shape 'L)))

    ;; assert initial setup
    (check block-lists=?
           (playfield-blocks (tetrion-playfield ft0))
           (strings->blocks '("..L."
                              "LLL."
                              "...."
                              "....")))

    (define ft1 (tetrion-rotate ft0 #t))
    (check block-lists=?
           (playfield-blocks (tetrion-playfield ft1))
           (strings->blocks '(".L.."
                              ".L.."
                              ".LL."
                              "...."))))
  (test-case
      "Rotate 90 with wall kick"
    (define ft2 (~> (new-tetrion #:cols 4 #:rows 2)
                    (tetrion-spawn-shape #:x -1 #:y 1 'L #:rotation 1)))
    ;; assert initial setup
    (check block-lists=?
           (playfield-blocks (tetrion-playfield ft2))
           (strings->blocks '("L..."
                              "L..."
                              "LL.."
                              "....")))
    (check block-lists=?
           (playfield-blocks (tetrion-playfield (tetrion-rotate ft2 #t)))
           (strings->blocks '("...."
                              "LLL."
                              "L..."
                              "...."))))
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
          (tetrion-spawn-shape 'L #:x 0 #:y 0 #:rotation 0)))

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
    (define ft0 (~> (new-tetrion #:cols 4 #:rows 2
                                 #:locked-blocks (strings->blocks '("JJ..")))
                    (tetrion-spawn-shape 'I)))

    ;; check assumption
    (check block-lists=?
           (playfield-blocks (tetrion-playfield ft0))
           (strings->blocks '("IIII"
                              "...."
                              "JJ..")))

    ;; Successful move
    (define ft0-drop (tetrion-move ft0 (make-posn 0 -1)))
    (check block-lists=?
           (playfield-blocks (tetrion-playfield ft0-drop))
           (strings->blocks '("IIII"
                              "JJ..")))

    ;; Fail on collision with locked blocks
    (check-exn
     exn:fail?
     (λ () (tetrion-move ft0-drop (make-posn 0 -1))))

    ;; Fail on collision with bottom of playfield
    (define ft1 (~> (new-tetrion #:cols 3 #:rows 0)
                    (tetrion-spawn-shape 'L)))
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
    (define tn-new (~> (new-tetrion) (tetrion-spawn)))
    (check-not-exn
     (λ () (tetrion-move tn-new (make-posn 0 -1))))
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
    (define ft0 (~> (new-tetrion #:cols 4 #:rows 2)
                    (tetrion-spawn-shape 'O)))
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
    (define ft0 (~> (new-tetrion #:cols 4 #:rows 3)
                    (tetrion-spawn-shape 'O)))
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
; Locks piece (adds the blocks to `locked`) and clears it (sets it to #f),
; and clears the full rows.
; Raises error on lock out, when all its blocks are in the vanish zone.
(define (tetrion-lock tn)
  (define rows (playfield-rows (tetrion-locked tn)))
  (define piece-min-y (~> (tetrion-piece tn)
                          (piece-blocks)
                          (map (λ (b) (posn-y (block-posn b))) _)
                          (apply min _)))
  (cond
    [(>= piece-min-y rows)
     (error "All piece blocks above ceiling: lock out")]
    [else
     (define piece-blcks (~> tn tetrion-piece piece-blocks))
     (define new-locked
       (~> (tetrion-locked tn)
           (playfield-add-blocks  piece-blcks)
           (playfield-clear-lines)))
     (struct-copy tetrion tn
                  [locked new-locked]
                  [piece #f])]))


(module+ test
  (test-case
      "Lock out if locked right after spawn (since pieces spawn above the ceiling)"
    (define ft0 (~> (new-tetrion )
                    (tetrion-spawn-shape 'L)))
    (check-exn
     #rx"lock out"
     (λ () (tetrion-lock ft0))))


  (test-case
      "No lock out if at least a single block below ceiling"
    (define tn-dropped
      (~> (new-tetrion)
          ;; assuming the I will be spawned on the bottom of the vanish zone
          (tetrion-spawn-shape 'I #:rotation 1)
          ;; move the I so 1 block is below vanish zone
          tetrion-drop))
    (check-not-exn
     (λ () (tetrion-lock tn-dropped)))))


(define (tetrion-add-garbage tion n)
  (define new-locked
    (playfield-add-garbage (tetrion-locked tion) n))
  (struct-copy tetrion tion
               [locked new-locked]))
