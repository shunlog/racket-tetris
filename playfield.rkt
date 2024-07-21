#lang racket/base

;; This module implements a Tetris Playfield,
;; which is the grid that you can add blocks to.
;;   To a Playfield, all the blocks are equivalent,
;; so it can't tell apart settled blocks
;; from blocks that make up the active piece

(require racket/contract)
(provide
 ;; Returns an empty Playfield of a given size (w, h)
 (contract-out
  [empty-playfield (-> natural-number/c natural-number/c playfield?)]

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

(define (playfield-width p)
  (playfield-w p))

(define (playfield-height p)
  (playfield-h p))



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

; Playifeld List[Block] -> Playfield
(define (playfield-add-block* p bl)
  (foldl (λ (b p) (playfield-add-block p b)) p bl))

; Playfield -> List[Block]
(define (playfield-blocks p)
  (define hash (playfield-block-hash p))
  (define (key-val-to-block k v)
    (block (car k) (cadr k) v))
  (hash-map hash key-val-to-block))



; Playfield -> Playfield
(define (playfield-clear-lines p)
  p)
