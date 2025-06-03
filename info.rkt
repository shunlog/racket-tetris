#lang info
(define collection "racket-tetris")
(define deps '("base" "threading" "memo"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/racket-tetris.scrbl" ())))
(define pkg-desc "Full Tetris implementation")
(define version "0.0")
(define pkg-authors '(awh))
(define license '(Apache-2.0 OR MIT))
