#lang racket/base

(require pict)
(require threading)
(require "playfield.rkt")
(require "block.rkt")
(require "draw.rkt")
(require "utils.rkt")

; Init the test module
(module+ test
  (require rackunit)
  (displayln "Running tests."))


(module+ test
  (test-case
      "Clear lines"
    (displayln "Clear lines")
    (define plf0
      (~> (empty-playfield 2 3)
          (playfield-add-blocks
           (strings->blocks '(".S"
                              ".."
                              "II"
                              "J."
                              "LL")))))
    (define-values (plf-cleared num-cleared) (playfield-clear-lines plf0))
    (check block-lists=?
           (playfield-blocks plf-cleared)
           (strings->blocks '(".S"
                              ".."
                              "J.")))

    (hc-append 10
               (playfield-pict plf0)
               (arrow 30 0)
               (playfield-pict plf-cleared))
    ))
