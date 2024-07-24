#lang racket/base

;; A Block is the basic building block of Tetris.
;; It is simply a structure that keeps track of the block's position and type, BlockType.

(require racket/contract)
(provide
 (contract-out
  [struct block ((x natural-number/c)
                 (y natural-number/c)
                 (type block-type?))]
  [BLOCK-TYPES (listof symbol?)]
  [strings->blocks (-> (listof (λ (s)
                                   (for/and ([ch s])
                                     (member (string->symbol (string ch))
                                             (cons '|.| BLOCK-TYPES)))))
                       (listof block?))]
  [block-move (-> block? posn? block?)]))


; -------------------------------
; Requires


(require rackunit)
(require "shapes.rkt")
(require lang/posn)


;; ----------------------------
;; Definitions

; A BlockType is either a ShapeName or one of '(ghost garbage)
(define BLOCK-TYPES
  (append SHAPE-NAMES '(ghost garbage)))

(define (block-type? t)
  (member t BLOCK-TYPES))


(struct block [x y type]
  #:transparent)


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
  (for/fold ([acc-ls '()])
            ([y (in-inclusive-range (sub1 (length sl)) 0 -1)]
             [s sl])
    (append acc-ls
            (for/list ([ch s]
                       [x (in-naturals)]
                       #:unless (equal? #\. ch))
              (block x y (string->symbol (string ch)))))))

(module+ test
  (test-case
     "strings->blocks"
   (define blocks
     (strings->blocks
      '("LJS.O"
        ".LIO")))
   (define expected
     (list
      (block 1 0 'L)
      (block 2 0 'I)
      (block 3 0 'O)
      (block 0 1 'L)
      (block 1 1 'J)
      (block 2 1 'S)
      (block 4 1 'O)))
   (for ([b blocks])
     (check-not-false
      (member b expected)
      (format "Block ~v not in the list of expected blocks" b)))))


; Block Posn -> Block
; Offset the block coordinates by the posn
(define (block-move blck posn)
  (define new-x (+ (posn-x posn) (block-x blck)))
  (define new-y (+ (posn-y posn) (block-y blck)))
  (if (or (< new-x 0) (< new-y 0))
      (error (format "Resulting block position is negative: ~v" (list new-x new-y)))
      (struct-copy block blck
                   [x new-x]
                   [y new-y])))

(module+ test
  (test-case
      "block-move: add Posn to Block"
      (check-equal?
       (block-move (block 1 1 'L) (make-posn 2 -1))
       (block 3 0 'L))))
