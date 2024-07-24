#lang racket/base

(require racket/set)
(require racket/contract)

(provide
 (contract-out
  [block-lists=? (-> (listof any/c) (listof any/c) boolean?)]))


;; List List -> Boolean
;; Check that two lists have the same elements, regardless of order,
;; assuming the elements are unique.
(define (block-lists=? bl1 bl2)
  (set=?
   (apply set bl1)
   (apply set bl2)))
