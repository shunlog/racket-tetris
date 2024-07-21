#lang racket/base

(require racket/contract)
(require "shapes.rkt")

(provide
 (contract-out
  (struct block ((x natural-number/c)
                 (y natural-number/c)
                 (type block-type?)))))

(define (block-type? t)
  (or (member t shape-names)
      (equal? t 'ghost)
      (equal? t 'garbage)))

(struct block [x y type]
  #:transparent)
