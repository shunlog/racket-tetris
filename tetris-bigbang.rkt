#lang racket/base


(provide
 tetris-run)


(define RATE 120)  ;; frames/second

(require 2htdp/universe)
(require threading)
(require "tetris.rkt")
(require "draw.rkt")
(require "filtered-autofire.rkt")


(define (millis)
  (~> (current-inexact-monotonic-milliseconds)
      floor
      inexact->exact))


(define (tetris-on-key t k ms)
  (cond
    [(key=? k "left") (tetris-left-pressed t ms)]
    [(key=? k "right") (tetris-right-pressed t ms)]
    [(key=? k "z") (tetris-rotate-ccw t ms)]
    [(or (key=? k "up") (key=? k "x")) (tetris-rotate-cw t ms)]
    [(key=? k " ") (tetris-hard-drop t ms)]
    [else t]))


(define (tetris-on-release t k ms)
  (cond
    [(key=? k "left") (tetris-left-released t ms)]
    [(key=? k "right") (tetris-right-released t ms)]
    [else t]))


(define-values (on-key-filtered on-release-filtered)
  (filtered-autofire))

(define (tetris-on-key-filtered ws k ms)
  (on-key-filtered ws k (λ () (tetris-on-key ws k ms))))

(define (tetris-on-release-filtered ws k ms)
  (on-release-filtered ws k (λ () (tetris-on-release ws k ms))))


(define (tetris-run)
  (big-bang (new-tetris (millis))
            [on-tick (λ (ws) (tetris-on-tick ws (millis)))
                     (/ 1 RATE)]
            [on-key (λ (ws k) (tetris-on-key-filtered ws k (millis)))]
            [on-release (λ (ws k) (tetris-on-release-filtered ws k (millis)))]
            [to-draw (λ (ws) (draw-tetris ws))]))
