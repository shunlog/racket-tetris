#lang racket/base

(require racket/contract)

(provide
 (contract-out
  (SHAPE-NAMES (listof symbol?))
  (shape-name? (-> any/c boolean?))))


(define SHAPE-NAMES
  '(L J S Z O I T))


(define (shape-name? sexp)
  (not (not (member sexp SHAPE-NAMES))))
