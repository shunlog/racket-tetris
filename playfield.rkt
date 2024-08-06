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


(require racket/list)
(require threading)
(require lang/posn)
(require "utils.rkt")
(require "block.rkt")


;; ----------------------------
;; Definitions


; A Playfield is a struct:
; - cols: natural
; - rows: natural
; - m: matrix, a list of `rows` lists, each having `cols` items;
;      a list item is either #f or a BlockType
(struct playfield [cols rows m])

;; Example 1
(define bt1 'garbage)
(define bt2 (cons 'L 'normal))
(define PLF1-blocks (list (block (make-posn 0 0) bt1)
                          (block (make-posn 1 1) bt2)))
(define PLF1 (playfield 2 2 `((,bt1 #f) (#f ,bt2))))


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


; Natural, Natural -> Playfield
(define (empty-playfield [cols 10] [rows 20])
  (playfield cols rows
             (build-list (* 2 rows)
                         (λ (_) (build-list cols
                                            (λ (_) #f))))))


; Playfield -> List[Block]
(define (playfield-blocks plf)
  (define (row-blocks row y)
    (for/list ([val row]
               [x (in-naturals)]
               #:when (block-type/c val))
      (block (make-posn x y) val)))
  (for/fold ([acc-ls '()])
            ([row (playfield-m plf)]
             [y (in-naturals)])
    (append acc-ls (row-blocks row y))))



(module+ test  
  (test-case
      "Create an empty Playfield"
    (define plf0 (empty-playfield 8 15))
    (check-equal? (playfield-blocks plf0) '())
    (check-equal? (playfield-cols plf0) 8)
    (check-equal? (playfield-rows plf0) 15))

  (test-case
      "Get playfield blocks"
    (check-equal?
     (playfield-blocks PLF1)
     PLF1-blocks))
  )


; Playfield (or/c Block (listof Block)) -> Boolean
(define (playfield-can-place? plf block-or-list)
  (define (can-place-block? block)
    (define-values (x y) (values (posn-x (block-posn block))
                                 (posn-y (block-posn block))))
    (define m (playfield-m plf))
    (define row (list-ref m y))
    (cond
      [(>= x (playfield-cols plf)) #f]
      [(>= y (* 2 (playfield-rows plf))) #f]
      [(not (list-ref row x)) #t]
      [else #f]))
  
  (cond
    [(list? block-or-list)
     (for/and ([block block-or-list])
       (can-place-block? block))]
    [(block? block-or-list)
     (can-place-block? block-or-list)]
    [else (error "Invalid argument")]))


; Playfield Block -> Playfield
(define (playfield-add-block plf block)
  (if (not (playfield-can-place? plf block))
      (error "Can't place block: " block)
      #f)  
  (define-values (x y) (values (posn-x (block-posn block))
                               (posn-y (block-posn block))))
  (define m (playfield-m plf))
  (define row (list-ref m y))
  (define new-row (list-set row x (block-type block)))
  (define new-m (list-set m y new-row))
  (struct-copy playfield plf [m new-m]))


; Playifeld List[Block] -> Playfield
(define (playfield-add-blocks p bl)
  (foldl (λ (b p) (playfield-add-block p b)) p bl))


(module+ test
  (define B1 (block (make-posn 1 1) (cons 'L 'normal)))
  (define B2 (block (make-posn 1 2) (cons 'J 'normal)))
  (test-case
      "Add a block to the Playfield"
    (define pl0 (~> (empty-playfield 10 20)
                    (playfield-add-block B1)))
    
    (check-equal? (playfield-blocks pl0) (list B1)))

  (test-case
      "Add a list of blocks"
    (define pl2 (~> (empty-playfield 10 20)
                    (playfield-add-blocks (list B1 B2))))
    (check block-lists=?
           (playfield-blocks pl2)
           (list B1 B2)))

  (test-case
      "Expect error when there is a block at given position already"
    (define pl3 (~> (empty-playfield 10 20)
                    (playfield-add-block B1)))
    (check-exn
     #rx"pos.*1.*1"
     (λ () (playfield-add-block pl3 B1))))

  (test-case
      "Error on adding block outside playfield"
    (define plf0 (empty-playfield 1 2))
    (define b0 (block (make-posn 1 0) (cons 'J 'normal)))
    (check-exn
     #rx"pos.*1.*0"
     (λ () (playfield-add-block plf0 b0))))
  
  (test-case
      "Add block in the vanish zone (which is the same size as the active zone)"
    (define plf0 (empty-playfield 1 2))
    (define b0 (block (make-posn 0 3) (cons 'L 'normal)))
    (check-not-exn
     (λ () (playfield-add-block plf0 b0))))
)


; Playfield -> Playfield
(define (playfield-clear-lines p)
  p)

(module+ test
  (test-case
      "Clear lines"
    (define plf0
      (~> (empty-playfield 2 3)
          (playfield-add-blocks
           (strings->blocks '(".S"
                              ".."
                              "II"
                              "J."
                              "LL")))))
    (check block-lists=?
           (playfield-blocks (playfield-clear-lines plf0))
           (strings->blocks '(".S"
                              ".."
                              "J.")))))
