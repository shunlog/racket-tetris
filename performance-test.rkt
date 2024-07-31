#lang racket/base

(require "tetris-bigbang.rkt")
(require "tetris.rkt")
(require "tetrion.rkt")
(require "block.rkt")
(require "draw.rkt")


(define rows 20)
(define cols 10)

(define big-tetrion
  (new-tetrion #:rows rows
               #:cols cols
               #:locked-blocks (for*/list ([x (in-range cols)]
                                           [y (in-range (- rows 10))])
                                 (block x y 'L #f))))


(draw-tetrion big-tetrion)

 
;; (require profile)
;; (require profile/render-graphviz)
;; (profile #:render render
;;          (tetris-run #:tetrion big-tetrion))

 
(require contract-profile)
(contract-profile (tetris-run #:tetrion big-tetrion))
