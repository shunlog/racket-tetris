#lang racket/base

(require 2htdp/universe)
(require 2htdp/image)
(require threading)
(require "tetris.rkt")
(require "draw.rkt")


(define (millis)
  (~> (current-inexact-monotonic-milliseconds)
      floor
      inexact->exact))


(define (tetris-on-key t k ms)
  (cond
    [(key=? k "left") (tetris-pressed-left t ms)]
    [(key=? k "right") (tetris-pressed-right t ms)]
    [(key=? k "down") (tetris-drop t ms)]
    [else t]))

(big-bang (new-tetris)
          [on-key (λ (t k) (tetris-on-key t k (millis)))]
          [to-draw
           (λ (t)
             (draw-tetris t))])
