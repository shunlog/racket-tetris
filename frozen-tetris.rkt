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

;; FrozenTetris is supposed to be wrapped by a Tetris "driver",
;; which will handle the rest of the logic.
;; Because of that, it's useful to make some functions raise exceptions,
;; for example to make `frozen-tetris-drop` raise an exception
;; when the piece is already on the floor.
;; This way, the Tetris driver can immediately know that it's time to lock the piece,
;; and doesn't have to double check if the piece has moved, for example,
;; as it would be the case if the drop function returned the unmodified frozen-tetris.



(require racket/contract)
(provide
 (contract-out
  [new-frozen-tetris (-> frozen-tetris?)]
  [frozen-tetris? (-> any/c boolean?)]
  [frozen-tetris-playfield (-> frozen-tetris? playfield?)]
  [frozen-tetris-drop (-> frozen-tetris? frozen-tetris?)]
  [frozen-tetris-right (-> frozen-tetris? frozen-tetris?)]
  [frozen-tetris-left (-> frozen-tetris? frozen-tetris?)]))


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
(struct frozen-tetris [piece locked shape-generator])


; A Piece is a struct:
; - posn: Posn
; - shape-name: one of shape-names
(struct piece [posn shape-name rotation])

; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(define (frozen-tetris-spawn ft [shape-name #f])
  (define sname
    (if (not shape-name)
        ((frozen-tetris-shape-generator ft))
        shape-name))
  (define grid-cols
    (~> ft frozen-tetris-locked playfield-cols))
  (define grid-rows
    (~> ft frozen-tetris-locked playfield-rows))
  (define piece-x
    (floor (/ (- grid-cols (shape-width sname)) 2)))
  (define y-spawn
    ; the lowest blocks should spawn on the first line of the vanish zone,
    ; so if there are 20 rows, it should spawn on row with index 20 (0-based)
    grid-rows)
  (define piece-y
    (- y-spawn (shape-gap-below sname)))
  (define new-piece
    (piece (make-posn piece-x piece-y)
           sname
           0))
  (struct-copy frozen-tetris ft
               [piece new-piece]))


(module+ test
  (test-case
      "Spawn a piece"
    (define ft0
      (frozen-tetris (piece (make-posn 0 0) 'L 0)
                     (empty-playfield 10 2)
                     7-loop-shape-generator))
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

    (check
     block-lists=?
     (~> ft0
         (frozen-tetris-spawn 'I)
         frozen-tetris-playfield
         playfield-blocks)
     (strings->blocks '("...IIII..."
                        ".........."
                        "..........")))
    ))


(define (new-frozen-tetris [shape-generator 7-loop-shape-generator])
  (frozen-tetris-spawn
   (frozen-tetris
    #f
    (empty-playfield)
    shape-generator)))


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
  (playfield-add-blocks
   locked
   (piece-blocks piece)))


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
       plf1
       7-loop-shape-generator))
    (check
     block-lists=?
     (playfield-blocks (frozen-tetris-playfield ft1))
     (playfield-blocks (playfield-add-blocks
                        plf1
                        (strings->blocks '(".T."
                                           "TTT"
                                           "...")))))))


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
           (playfield-add-blocks (strings->blocks '("JJ.."))))
       7-loop-shape-generator))

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
       (empty-playfield 3 3)
       7-loop-shape-generator))    
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
     (λ () (frozen-tetris-move (new-frozen-tetris) (make-posn 0 -1))))
    ))


(define (frozen-tetris-drop ft)
  (frozen-tetris-move ft (make-posn 0 -1)))

(define (frozen-tetris-right ft)
  (frozen-tetris-move ft (make-posn 1 0)))

(define (frozen-tetris-left ft)
  (frozen-tetris-move ft (make-posn -1 0)))
