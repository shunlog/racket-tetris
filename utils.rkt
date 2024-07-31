#lang racket/base

(require racket/set)
(require racket/contract)


(provide
 (contract-out
  [block-lists=? (-> (listof any/c) (listof any/c) boolean?)]
  [matrix-rotate-cw (-> (listof (listof any/c))
                        (listof (listof any/c)))]
  [de-nest (-> (listof any/c) (listof any/c))]))


;; List List -> Boolean
;; Check that two lists have the same elements, regardless of order,
;; assuming the elements are unique.
(define (block-lists=? bl1 bl2)
  (set=?
   (apply set bl1)
   (apply set bl2)))


; List of Lists -> List of Lists
; Transpose a matrix
(define (matrix-transpose ll)
  (apply map list ll))


; List of Lists -> List of Lists
; Rotate matrix 90 degrees
(define (matrix-rotate-cw ll)
  (map reverse (matrix-transpose ll)))


; List -> List
; Flatten one level of nesting
(define (de-nest lss)
  (foldr
   (λ (ls xs)
     (foldr cons xs ls))
   '() lss))
