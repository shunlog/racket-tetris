#lang racket/base

(require rackunit)

(require "block.rkt")
(require "playfield.rkt")


(test-case
    "Create an empty Playfield"
  (define plf0 (empty-playfield 10 20))
  (check-equal? (playfield-blocks plf0) '())
  (check-equal? (playfield-width plf0) 10)
  (check-equal? (playfield-height plf0) 20))


(test-case
    "Add blocks to Playfield and retrieve them"
  (define pl0 (empty-playfield 10 20))

  ; Add a single block
  (define b1 (block 1 1 'L))  
  (define pl1 (playfield-add-block pl0 b1))
  (check-equal?
   (playfield-blocks pl1)
   `(,b1))

  ; Add list of blocks
  (define bl1 `(,(block 0 1 'J)
                ,(block 1 2 'S)))
  (define pl2 (playfield-add-block* pl0 bl1))
  (check-not-false
   (andmap (λ (b) (member b (playfield-blocks pl2)))
           bl1)))


(test-case
    "Error when there already are blocks at given positions."
  (define pl0 (empty-playfield 10 20))

  ; Test adding one block  
  (define pl1 (playfield-add-block
               pl0
               (block 1 1 'L)))
  (check-exn
   #rx"position.*1.*1"
   (λ () (playfield-add-block
          pl1
          (block 1 1 'L))))

  ; Test adding a list of blocks
  (check-exn
   #rx"position.*1.*2"
   (λ () (playfield-add-block*
          pl0
          `(,(block 1 2 'L)
            ,(block 1 2 'J))))))
