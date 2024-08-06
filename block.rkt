#lang racket/base

;; A Block represents a filled square in the Tetris grid,
;; it can be locked, a garbage block, part of the Piece, or even a ghost block.


(require racket/contract)
(provide
 (contract-out
  [struct block ((posn nat-posn?) (type block-type/c))]
  [block-type/c contract?]
  [strings->blocks (-> (listof (λ (s)
                                 (for/and ([ch s])
                                   (or (shape-name/c (string->symbol (string ch)))
                                       (equal? ch #\.)))))
                       (listof block?))]
  [block-move (-> block? posn? block?)]
  [blocks-max-x (-> (listof block?) number?)]
  [blocks-min-x (-> (listof block?) number?)]
  [blocks-min-y (-> (listof block?) number?)]))


; -------------------------------
; Requires


(require rackunit)
(require lang/posn)
(require "shapes.rkt")
(require "utils.rkt")



;; ----------------------------
;; Definitions


;; A NatPosn is a Posn of 2 natural numbers
(define (nat-posn? p)
  (and (posn? p)
       (natural-number/c (posn-x p))
       (natural-number/c (posn-y p))))


;; A BlockType is either:
;; - 'garbage
;; - a cons of ShapeName and one of '(normal ghost piece)
(define block-type/c
  (or/c (λ (t) (equal? 'garbage t))
        (cons/c shape-name/c (one-of/c 'normal 'ghost 'piece))))


; A Block is a struct:
; - posn: a NatPosn that represents its coordinates in the Playfield,
;         with the origin at the bottom-left corner
; - type: a BlockType, defines how it will be drawn
(struct block [posn type]
  #:transparent)
(define block/c
  (struct/c block nat-posn? block-type/c))


;; Examples:
(define GARBAGE-BLOCK (block (make-posn 0 0) 'garbage))
(define L-GHOST-BLOCK (block (make-posn 5 6) (cons 'L 'ghost)))


; -------------------------------
; Implementation


; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


; Convert a list-of-strings graphical representation of a playfield
; to a list of blocks.
; Each character is a block, and is taken as the block type,
; except for the "." which signifies an empty space
(define (strings->blocks sl)
  (define (string->blocks s y)
    (for/list ([ch s]
               [x (in-naturals)]
               #:unless (equal? #\. ch))
      (define shape-name (string->symbol (string ch)))
      (block (make-posn x y) (cons shape-name 'normal))))
  (for/fold ([acc-ls '()])
            ([y (in-inclusive-range (sub1 (length sl)) 0 -1)]
             [s sl])
    (append acc-ls
            (string->blocks s y))))

(module+ test
  (test-case
      "strings->blocks"
    (define blocks
      (strings->blocks
       '("LJS.O"
         ".LIO")))
    (define expected
      (list
       (block (make-posn 1 0) (cons 'L 'normal))
       (block (make-posn 2 0) (cons 'I 'normal))
       (block (make-posn 3 0) (cons 'O 'normal))
       (block (make-posn 0 1) (cons 'L 'normal))
       (block (make-posn 1 1) (cons 'J 'normal))
       (block (make-posn 2 1) (cons 'S 'normal))
       (block (make-posn 4 1) (cons 'O 'normal))))
    (for ([b blocks])
      (check-not-false
       (member b expected)
       (format "Block ~v not in the list of expected blocks" b)))))


; Block Posn -> Block
; Offset the block coordinates by the posn
(define (block-move blck posn)
  (define blck-posn (block-posn blck))
  (define new-posn (posn+ blck-posn posn))
  (if (not (nat-posn? new-posn))
      (error (format "Resulting block position is negative: ~v" new-posn))
      (struct-copy block blck
                   [posn new-posn])))

(module+ test
  (test-case
      "block-move: add Posn to Block"
    ;; Works fine
    (check-equal?
     (block-move (block (make-posn 1 1) (cons 'L 'normal)) (make-posn 2 -1))
     (block (make-posn 3 0) (cons 'L 'normal)))

    ;; Error if pos is negative
    (check-exn
     exn:fail?
     (λ () (block-move (block (make-posn 0 0) (cons 'L 'normal))
                       (make-posn -1 0))))
    ))


(define (blocks-max-x bl)
  (apply max (for/list ([b bl])
               (posn-x (block-posn b)))))

(define (blocks-min-x bl)
  (apply min (for/list ([b bl])
               (posn-x (block-posn b)))))

(define (blocks-min-y bl)
  (apply min (for/list ([b bl])
               (posn-y (block-posn b)))))
