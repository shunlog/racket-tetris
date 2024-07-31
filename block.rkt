#lang racket/base

;; A Block is the basic building block of Tetris.
;; It is simply a structure that keeps track of the block's position and type, BlockType.

(require racket/contract)
(provide
 (contract-out
  [struct block ((x natural-number/c)
                 (y natural-number/c)
                 (type block-type?)
                 (ghost boolean?))]
  [strings->blocks (-> (listof (λ (s)
                                 (for/and ([ch s])
                                   (or (shape-name? (string->symbol (string ch)))
                                       (equal? ch #\.)))))
                       (listof block?))]
  [block-move (-> block? posn? block?)]))


; -------------------------------
; Requires


(require rackunit)
(require "shapes.rkt")
(require lang/posn)


;; ----------------------------
;; Definitions


; A Block is a struct:
; - x, y are coordinates in the Playfield
; - ghost is a Boolean
; - type is either a ShapeName or 'garbage
(struct block [x y type ghost]
  #:transparent)

(define (block-type? t)
  (or (shape-name? t)
      (equal? t 'garbage)))


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
              (block x y (string->symbol (string ch)) #f)))))

(module+ test
  (test-case
     "strings->blocks"
   (define blocks
     (strings->blocks
      '("LJS.O"
        ".LIO")))
   (define expected
     (list
      (block 1 0 'L #f)
      (block 2 0 'I #f)
      (block 3 0 'O #f)
      (block 0 1 'L #f)
      (block 1 1 'J #f)
      (block 2 1 'S #f)
      (block 4 1 'O #f)))
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
    ;; Works fine
    (check-equal?
     (block-move (block 1 1 'L #f) (make-posn 2 -1))
     (block 3 0 'L #f))

    ;; Error if pos is negative
    (check-exn
     exn:fail?
     (λ () (block-move (block 0 0 'L #f) (make-posn -1 0))))
    ))
