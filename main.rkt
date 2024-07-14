#lang racket/base

(require "tetris.rkt")
(require "utils.rkt")
(require 2htdp/universe)


(define (main tick-durn)
  (big-bang (tetris-init)
            [on-tick tetris-on-tick tick-durn]
            [to-draw draw-any-blocks]
            [on-key (λ (ws k) (tetris-on-key ws k (time-ms)))]
            [on-release tetris-on-release]))

(main 0.02)
