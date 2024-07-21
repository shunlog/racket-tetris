#lang racket/base

(require racket/contract)

(provide
 (contract-out
  (shape-names (or/c (listof symbol?)))))

(define shape-names
  '(L J S Z O I T))
