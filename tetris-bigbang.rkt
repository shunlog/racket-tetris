#lang racket/base


(provide
 tetris-run)


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
    [(key=? k " ") (tetris-hard-drop t ms)]
    [else t]))


(define (tetris-run)
  (big-bang (new-tetris (millis))
            [on-tick (λ (t) (tetris-on-tick t (millis)))]
            [on-key (λ (t k) (tetris-on-key t k (millis)))]
            [to-draw
             (λ (t)
               (draw-tetris t))]))
