#lang racket/base

(require "tetris.rkt")
(require "utils.rkt")
(require 2htdp/universe)


(define (main tick-durn)
  (big-bang (tetris-init (time-ms))
            [check-with tetris?]
            [to-draw draw-any-blocks]
            [on-tick
             (λ (ws) (tetris-on-tick ws (time-ms)))
             tick-durn]
            [on-key
             (λ (ws k) (tetris-on-key ws k (time-ms)))]
            [on-release
             (λ (ws k) (tetris-on-release ws k (time-ms)))]))

(main 0.02)
