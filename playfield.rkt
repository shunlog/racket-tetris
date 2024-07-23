#lang racket/base

;; This module implements a Tetris Playfield,
;; which is the grid that you can add blocks to.
;;   To a Playfield, all the blocks are equivalent,
;; so it can't tell apart settled blocks
;; from blocks that make up the active piece

(require racket/contract)
(provide
 (contract-out
  ;; Returns an empty Playfield of a given size (w, h)
  [empty-playfield (-> natural-number/c natural-number/c playfield?)]

  [playfield? any/c]

  ;; Field accessors
  [playfield-width (-> playfield? natural-number/c)]
  [playfield-height (-> playfield? natural-number/c)]

  ;; Add blocks to a Playfield
  [playfield-add-block (-> playfield? block? playfield?)]
  [playfield-add-block* (-> playfield? (listof block?) playfield?)]

  ;; Get a list of Blocks in the Playfield
  [playfield-blocks (-> playfield? (listof block?))]

  ;; Clear completed lines in the Playfield
  [playfield-clear-lines (-> playfield? playfield?)]
  ))


; -------------------------------
; Requires

(require racket/set)
(require "block.rkt")

;; ----------------------------
;; Definitions


; A Playfield is a Struct:
; - w (width): natural
; - h (height): natural
; - bset (blocks data)
(struct playfield [w h block-hash])


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


; Natural, Natural -> Playfield
(define (empty-playfield w h)  (playfield w h (hash)))


; Playfield -> List[Block]
(define (playfield-blocks p)
  (define hash (playfield-block-hash p))
  (define (key-val-to-block k v)
    (block (car k) (cadr k) v))
  (hash-map hash key-val-to-block))


(define (playfield-width p)
  (playfield-w p))


(define (playfield-height p)
  (playfield-h p))


(module+ test  
  (test-case
      "Create an empty Playfield"
    (define plf0 (empty-playfield 10 20))
    (check-equal? (playfield-blocks plf0) '())
    (check-equal? (playfield-width plf0) 10)
    (check-equal? (playfield-height plf0) 20))
  )


; Playfield Block -> Playfield
(define (playfield-add-block p block)
  (define x (block-x block))
  (define y (block-y block))
  (define hash (playfield-block-hash p))
  (cond
    [(hash-ref hash (list x y) #f)
     (error "There already exists a block at given position:" (list x y))]
    [else
     (define btype (block-type block))

     (define new-hash
       (hash-set hash `(,x ,y) btype))
     (struct-copy playfield p
                  [block-hash new-hash])]))


(module+ test
  (test-case
      "Add a block to the Playfield"
    (define pl0 (empty-playfield 10 20))

    ; Add a single block
    (define pl1 (playfield-add-block pl0 (block 1 1 'L)))
    (check-equal?
     (playfield-blocks pl1)
     (list (block 1 1 'L)))

    ; Add two blocks
    (define pl2 (playfield-add-block
                 (playfield-add-block pl0 (block 1 1 'L))
                 (block 1 2 'J)))
    (check-true
     (set=?  ;; we don't care about the order, so we create sets instead of lists
      (apply set (playfield-blocks pl2))
      (set (block 1 1 'L) (block 1 2 'J))))
    
    ;; Expect error when thereis a blocks at given position already
    (define pl3 (playfield-add-block pl0 (block 1 1 'L)))
    (check-exn
     #rx"position.*1.*1"
     (λ () (playfield-add-block pl3 (block 1 1 'J))))
    ))


; Playifeld List[Block] -> Playfield
(define (playfield-add-block* p bl)
  (foldl (λ (b p) (playfield-add-block p b)) p bl))


(module+ test
  (test-case
      "Add a list of blocks"
    (define pl0 (empty-playfield 10 20))
    
    ; Add list of blocks
    (define bl1 (list (block 0 1 'J)
                      (block 1 2 'S)))
    (define pl2 (playfield-add-block* pl0 bl1))
    (check-not-false
     (set=?
      (apply set (playfield-blocks pl2))
      (apply set bl1)))
    
    ;; Error if any block fails to be added (so none will be added)
    (check-exn
     #rx"position.*1.*2"
     (λ () (playfield-add-block* pl0 (list (block 1 2 'L)
                                           (block 1 2 'J)))))
    ))


; Playfield -> Playfield
(define (playfield-clear-lines p)
  p)
