#lang racket/base

;; A Playfield keeps track of the grid size and the Blocks in it.
;; The Playfield doesn't distinguish between blocks (whether active, locked or ghost).


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
  [playfield-add-blocks-maybe (-> playfield? (listof block?) playfield?)]
  [playfield-add-garbage (-> playfield? natural-number/c playfield?)]
  
  ;; Get a list of Blocks in the Playfield
  [playfield-blocks (-> playfield? (listof block?))]
  [playfield-block-matrix (-> playfield? (listof (listof (or/c tile? #f))))]
  
  ;; Clear completed lines in the Playfield
  [playfield-clear-lines (-> playfield? (values playfield? natural-number/c))]
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
;      a list item is either #f or a Tile
(struct playfield [cols rows m])

;; Example 1
(define B1 (block (make-posn 0 0) TILE-GARBAGE))
(define B2 (block (make-posn 1 1) TILE-GHOST))

(define PLF1-blocks (list B1 B2))
(define PLF1 (playfield 2 2 `((,TILE-GARBAGE #f) (#f ,TILE-GHOST))))


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


; Natural, Natural -> Playfield
(define (empty-playfield [cols 10] [rows 20])
  (playfield cols rows
             (build-list (max (+ rows 20) (* 2 rows)) ; at least 20 blocks of vanish zone, or double
                         (λ (_) (build-list cols
                                            (λ (_) #f))))))

(module+ test
  (test-case
      "Create an empty Playfield"
    (define plf0 (empty-playfield 8 15))
    (check-equal? (playfield-blocks plf0) '())
    (check-equal? (playfield-cols plf0) 8)
    (check-equal? (playfield-rows plf0) 15))
  )


; Playfield -> List[Block]
(define (playfield-blocks plf)
  (define (row-blocks row y)
    (for/list ([val row]
               [x (in-naturals)]
               #:when (tile? val))
      (block (make-posn x y) val)))
  (for/fold ([acc-ls '()])
            ([row (playfield-m plf)]
             [y (in-naturals)])
    (append acc-ls (row-blocks row y))))

; Playfield -> List[List[ (or Tile #f)]]
; Note: also returns the vanish zone lines
(define (playfield-block-matrix plf)
  (playfield-m plf))


(module+ test
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
      [(>= y (length m)) #f]
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
  (unless (playfield-can-place? plf block)
    (error "Can't place block: " block))
  (define-values (x y) (values (posn-x (block-posn block))
                               (posn-y (block-posn block))))
  (define m (playfield-m plf))
  (define row (list-ref m y))
  (define new-row (list-set row x (block-tile block)))
  (define new-m (list-set m y new-row))
  (struct-copy playfield plf [m new-m]))


; Playifeld List[Block] -> Playfield
; Raises exception if at least one block can't be added.
(define (playfield-add-blocks p bl)
  (foldl (λ (b p) (playfield-add-block p b)) p bl))



(module+ test

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
     #rx"pos.*0.*0"
     (λ () (playfield-add-block pl3 B1))))

  (test-case
      "Error on adding block outside playfield"
    (define plf0 (empty-playfield 1 2))
    ;; Make a block on column 2
    (define b0 (block (make-posn 1 0) TILE-GARBAGE))
    ;; Error should raise because there's only 1 column
    (check-exn
     #rx"pos.*1.*0"
     (λ () (playfield-add-block plf0 b0))))

  (test-case
      "Add block above the playfield's limits"
    (define plf0 (empty-playfield 1 2))
    (define b0 (block (make-posn 0 3) TILE-GARBAGE))
    (check-not-exn
     (λ () (playfield-add-block plf0 b0))))
  )


; Like playfield-add-blocks,
; but doesn't raise an exception when any block can't be added,
; and instead tries to add as many as possible
(define (playfield-add-blocks-maybe plf bl)
  (for/fold ([plf-res plf])
            ([b bl])
    (with-handlers ([exn:fail? (λ (_) plf-res)])
      (playfield-add-block plf-res b))))


;; Playfield  Non-zero-Natural -> Playfield
;; Add `n` lines of garbage to the playfield 
(define (playfield-add-garbage plf n)
  (define cols (playfield-cols plf))
  (define garbage-lines
    (build-list n (λ (_)
                    (append
                     (build-list (sub1 cols) (λ (_) TILE-GARBAGE))
                     '(#f)))))
  (define m (playfield-m plf))
  (define new-matrix
    (~> (append garbage-lines m)
        (take (length m))))
  (struct-copy playfield plf
               [m new-matrix]))


(module+ test
  (test-case
      "Add some blocks to the Playfield"
    (define B1 (block (make-posn 1 1) (tile-normal 'L)))
    (define B2 (block (make-posn 1 2) (tile-normal 'J)))
    (define B3 (block (make-posn 1 3) (tile-normal 'J)))
    (define pl0 (~> (empty-playfield 10 20)
                    (playfield-add-block B2)))
    (check block-lists=?
           (playfield-blocks (playfield-add-blocks-maybe pl0 (list B1 B2 B3)))
           (list B1 B2 B3)))
  )


; Playfield -> Playfield, Natural
; Clears full lines from the playfield,
; Returns the playfield alongside the number of cleared lines
(define (playfield-clear-lines plf)
  (define cols (playfield-cols plf))
  (define m (playfield-m plf))
  (define (full-row? r)
    (andmap (λ (v) v) r))
  (define filtered-m
    ;; filter the full rows
    (for/list ([row m] #:unless (full-row? row)) row))
  (define num-cleared (- (length m) (length filtered-m)))
  (define new-m
    ;; add back empty rows above
    (append filtered-m
            (build-list num-cleared
                        (λ (_) (build-list cols (λ (_) #f))))))
  (values (struct-copy playfield plf [m new-m])
          num-cleared))

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
    (define-values (plf-cleared num-cleared) (playfield-clear-lines plf0))
    (check block-lists=?
           (playfield-blocks plf-cleared)
           (strings->blocks '(".S"
                              ".."
                              "J.")))
    (check-equal? (length (playfield-m  plf-cleared))
                  (length (playfield-m plf0)))
    (check-equal? 2 num-cleared)))
