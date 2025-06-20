#lang racket/base

(require racket/set)
(require racket/contract)
(require lang/posn)


(provide
 exn:fail:tetris
 exn:fail:tetris?
 raise-tetris
 (contract-out
  [block-lists=? (-> (listof any/c) (listof any/c) boolean?)]
  [matrix-rotate-cw (-> (listof (listof any/c))
                        (listof (listof any/c)))]
  [de-nest (-> (listof any/c) (listof any/c))]
  [posn+ (-> posn? posn? posn?)]))


;;; Exception raised whenever the action breaks the rules of tetris,
;;; for example when a block is placed on top of another, or outside the playfield,
;;; or when the piece can't be moved in the given direction
(struct	exn:fail:tetris exn:fail ()
  #:extra-constructor-name make-exn:fail:tetris
  #:transparent)

(define (raise-tetris msg)
  (raise (make-exn:fail:tetris msg (current-continuation-marks))))


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

; Add two Posn's
(define (posn+ p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))
