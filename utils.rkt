#lang racket/base

(require rackunit)
(require racket/function)

(provide
 member?
 any-satisfies?
 random-choice
 de-nest
 matrix-rotate-cw
 count-less
 all-pairs-of-2-ranges)


; Number, List of Numbers -> Integer
; Count how many numbers in the list are smaller than the given number n
(define (count-less n l)
  (foldl + 0 (map (λ (x) (if (< x n) 1 0)) l)))
(check-equal? (count-less 5 '(0 2 5 6)) 2)


; Any List -> Bool
; returns True if value es in list
(define (member? el ls)
  (ormap [λ (s) (equal? s el)] ls))


; Function(Any -> Boolean) List -> Bool
; Returns true if any member of the list satisfies given function
; when passed as an argument to it
(define (any-satisfies? fun l)
  (ormap [λ (elem) (fun elem)] l))


; List -> Any
; Return a random item from the list
(define (random-choice l)
  (list-ref l (random (length l))))


; List of Lists -> List
; Flatten one level of nesting
(define (de-nest lss)
  (foldr
   (λ (ls xs)
     (foldr cons xs ls))
   '() lss))


; List of Lists -> List of Lists
; Transpose a matrix
(define (matrix-transpose ll)
  (apply map list ll))


; List of Lists -> List of Lists
; Rotate matrix 90 degrees
(define (matrix-rotate-cw ll)
  (map reverse (matrix-transpose ll)))


; Number, Number -> List of Pairs
; Return all pairs of items (i, j)
; such that i is taken from the range [0..N)
; and j is taken from the range [0..M)
(define (all-pairs-of-2-ranges N M)
  (de-nest
   (map (λ (y) (map list
                         (build-list N identity)
                         (build-list N (λ (_) y))))
        (build-list M identity))))
(check-equal? (all-pairs-of-2-ranges 2 4)
              '((0 0) (1 0) (0 1) (1 1) (0 2) (1 2) (0 3) (1 3)))
