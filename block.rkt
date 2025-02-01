#lang racket/base

;; A Block represents a tile (sprite) and its position.
;; A tile can be:
;; - normal colored tile
;; - faded "ghost" tile
;; - garbage tile

;; Note: A Block that's part of the active piece is no different from locked blocks.


(require racket/contract)
(provide
 (contract-out
  [tile? predicate/c]
  [tile-ghost? (-> tile? boolean?)]
  [tile-normal? (-> tile? boolean?)]
  [tile-garbage? (-> tile? boolean?)]
  [tile-normal (-> shape-name/c tile?)]
  [tile-ghost (-> shape-name/c tile?)]
  [tile-shape (-> tile? shape-name/c)]
  [TILE-GARBAGE tile?]
  [TILE-GHOST tile?]
  
  [struct block ((posn nat-posn?) (tile tile?))]
  [strings->blocks (-> block-strings/c (listof block/c))]
  [block-move (-> block? posn? block?)]))



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


;; A Tile is a struct:
;; - type: one of '(normal ghost garbage)
;; - shape: a Shape (or #f for the garbage tile)
(struct tile [type shape] #:transparent)

;; Examples
(define TILE-GHOST (tile 'ghost 'L))

;; Since there's only one kind of garbage tile, we'll export it as a constant
(define TILE-GARBAGE (tile 'garbage #f))

; Getters and setters for tiles
(define (tile-ghost? t)
  (equal? (tile-type t) 'ghost))
(define (tile-normal? t)
  (equal? (tile-type t) 'normal))
(define (tile-garbage? t)
  (equal? (tile-type t) 'garbage))

(define (tile-normal shape)
  (tile 'normal shape))
(define (tile-ghost shape)
  (tile 'ghost shape))



; A Block is a struct:
; - posn: a NatPosn that represents its coordinates in the Playfield,
;         with the origin at the bottom-left corner
; - type: a Tile, defines how it will be drawn
(struct block [posn tile] #:transparent)
(define block/c (struct/c block nat-posn? tile?))
;; Examples:
(define BLOCK-GARBAGE (block (make-posn 0 0) TILE-GARBAGE))
(define BLOCK-GHOST-L (block (make-posn 5 6) TILE-GHOST))


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
(define block-strings/c
  (listof (λ (s)
            (for/and ([ch s])
              (or (shape-name/c (string->symbol (string ch)))
                  (equal? ch #\.))))))
(define/contract (strings->blocks sl)
  (-> block-strings/c (listof block/c))
  (define (string->blocks s y)
    (for/list ([ch s]
               [x (in-naturals)]
               #:unless (equal? #\. ch))
      (define shape-name (string->symbol (string ch)))
      (block (make-posn x y) (tile-normal shape-name))))
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
       (block (make-posn 1 0) (tile-normal 'L))
       (block (make-posn 2 0) (tile-normal 'I))
       (block (make-posn 3 0) (tile-normal 'O))
       (block (make-posn 0 1) (tile-normal 'L))
       (block (make-posn 1 1) (tile-normal 'J))
       (block (make-posn 2 1) (tile-normal 'S))
       (block (make-posn 4 1) (tile-normal 'O))))
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
     (block-move (block (make-posn 1 1) (tile-normal 'L)) (make-posn 2 -1))
     (block (make-posn 3 0) (tile-normal 'L)))

    ;; Error if pos is negative
    (check-exn
     exn:fail?
     (λ () (block-move (block (make-posn 0 0) (tile-normal 'L))
                       (make-posn -1 0))))
    ))
