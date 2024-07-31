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
  [empty-playfield (->* () (natural-number/c natural-number/c) playfield?)]

  [playfield? any/c]

  ;; Field accessors
  [playfield-cols (-> playfield? natural-number/c)]
  [playfield-rows (-> playfield? natural-number/c)]

  ;; Add blocks to a Playfield
  [playfield-can-place? (-> playfield? (or/c block? (listof block?)) boolean?)]
  [playfield-add-block (-> playfield? block? playfield?)]
  [playfield-add-blocks (-> playfield? (listof block?) playfield?)]

  ;; Get a list of Blocks in the Playfield
  [playfield-blocks (-> playfield? (listof block?))]

  ;; Clear completed lines in the Playfield
  [playfield-clear-lines (-> playfield? playfield?)]
  ))


; -------------------------------
; Requires


(require "utils.rkt")
(require "block.rkt")


;; ----------------------------
;; Definitions


; A Playfield is a Struct:
; - cols: natural
; - rows: natural
; - bset (blocks data)
(struct playfield [cols rows block-hash])


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


; Natural, Natural -> Playfield
(define (empty-playfield [w 10] [h 20])  (playfield w h (hash)))


; Playfield -> List[Block]
(define (playfield-blocks p)
  (define hash (playfield-block-hash p))
  (define (key-val-to-block k v)
    (block (car k) (cadr k) v #f))
  (hash-map hash key-val-to-block))



(module+ test  
  (test-case
      "Create an empty Playfield"
    (define plf0 (empty-playfield 8 15))
    (check-equal? (playfield-blocks plf0) '())
    (check-equal? (playfield-cols plf0) 8)
    (check-equal? (playfield-rows plf0) 15))
  )


; Playfield (or/c Block (listof Block)) -> Boolean
(define (playfield-can-place? p block-or-list)
  (define (can-place-block? block)
    (define x (block-x block))
    (define y (block-y block))
    (define hash (playfield-block-hash p))
    (cond
      [(hash-ref hash (list x y) #f) #f]
      [(>= x (playfield-cols p)) #f]
      [(< x 0) #f]
      [(< y 0) #f]
      [else #t]))
  (cond
    [(list? block-or-list)
     (for/and ([block block-or-list])
       (can-place-block? block))]
    [(block? block-or-list)
     (can-place-block? block-or-list)]
    [else (error "Invalid argument")]))

(module+ test  
  (test-case
      "playfield-can-place?"
    (define plf0 (empty-playfield 3 3))

    ;; Can place above ceiling (vanish zone
    (check-true
     (playfield-can-place? plf0 (block 1 4 'L #f)))
    
    ;; Collides with block
    (define plf1 (playfield-add-block plf0 (block 1 1 'L #f)))
    (check-false
     (playfield-can-place? plf1 (block 1 1 'J #f)))
    
    ;; Out of bounds
    (check-false
     (playfield-can-place? plf0 (block 3 0 'J #f)))

    ;; Works on lists too
    (check-false
     (playfield-can-place? plf1 (list (block 0 0 'J #f) (block 1 1 'L #f))))
    ))


; Playfield Block -> Playfield
(define (playfield-add-block p block)
  (define x (block-x block))
  (define y (block-y block))
  (define hash (playfield-block-hash p))
  (cond
    [(not (playfield-can-place? p block))
     (error "Can't place block at position " (list x y))]
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
    (define pl1 (playfield-add-block pl0 (block 1 1 'L #f)))
    (check-equal?
     (playfield-blocks pl1)
     (list (block 1 1 'L #f)))

    ; Add two blocks
    (define pl2 (playfield-add-block
                 (playfield-add-block pl0 (block 1 1 'L #f))
                 (block 1 2 'J #f)))
    (check-true (block-lists=? (playfield-blocks pl2)
                    (list (block 1 1 'L #f) (block 1 2 'J #f))))
    
    ;; Expect error when thereis a blocks at given position already
    (define pl3 (playfield-add-block pl0 (block 1 1 'L #f)))
    (check-exn
     #rx"position.*1.*1"
     (λ () (playfield-add-block pl3 (block 1 1 'J #f))))
    ))


; Playifeld List[Block] -> Playfield
(define (playfield-add-blocks p bl)
  (foldl (λ (b p) (playfield-add-block p b)) p bl))


(module+ test
  (test-case
      "Add a list of blocks"
    (define pl0 (empty-playfield 10 20))
    
    ; Add list of blocks
    (define bl1 (list (block 0 1 'J #f)
                      (block 1 2 'S #f)))
    (define pl2 (playfield-add-blocks pl0 bl1))
    (check-true
     (block-lists=? (playfield-blocks pl2) bl1))
    
    ;; Error if any block fails to be added (so none will be added)
    (check-exn
     #rx"position.*1.*2"
     (λ () (playfield-add-blocks pl0 (list (block 1 2 'L #f)
                                           (block 1 2 'J #f)))))
    ))


; Playfield -> Playfield
(define (playfield-clear-lines p)
  p)
