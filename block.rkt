#lang racket/base

(require racket/contract)
(require rackunit)
(require "shapes.rkt")

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
                         (listof block?))]))

(define BLOCK-TYPES
  (append SHAPE-NAMES '(ghost garbage)))

(define (block-type? t)
  (member t BLOCK-TYPES))

(struct block [x y type]
  #:transparent)


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
     (format "Block ~v not in the list of expected blocks" b))))
