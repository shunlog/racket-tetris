#lang racket/base


(provide
 tetris-run)


(require 2htdp/universe)
(require threading)
(require "tetris.rkt")
(require "draw.rkt")


(define (millis)
  (~> (current-inexact-monotonic-milliseconds)
      floor
      inexact->exact))


(define key-pressed-hash (make-hash))


(define (on-key-filtered ws k f ms)
  (define pressed? (hash-ref key-pressed-hash k #f))
  (cond
    [pressed? ws]
    [else
     (hash-set! key-pressed-hash k #t)
     (f ws k ms)]))


(define (on-release-filtered ws k f ms)
  ;; Although pressed? will never be false in normal conditions,
  ;; I'll handle that just in case
  (define pressed? (hash-ref key-pressed-hash k #f))
  (cond
    [pressed?
     (hash-set! key-pressed-hash k #f)
     (f ws k ms)]
    [else ws]))


(define (tetris-on-key t k ms)
  (cond
    [(key=? k "left") (tetris-pressed-left t ms)]
    [(key=? k "right") (tetris-pressed-right t ms)]
    [(key=? k "z") (tetris-rotate-ccw t ms)]
    [(or (key=? k "up") (key=? k "x")) (tetris-rotate-cw t ms)]
    [(key=? k " ") (tetris-hard-drop t ms)]
    [else t]))


(define (tetris-on-release t k ms)
  (cond
    [else t]))


(define (tetris-run)
  (big-bang (new-tetris (millis))
            [on-tick (λ (ws) (tetris-on-tick ws (millis)))]
            [on-key (λ (ws k) (on-key-filtered ws k tetris-on-key (millis)))]
            [on-release (λ (ws k) (on-release-filtered ws k tetris-on-release (millis)))]
            [to-draw
             (λ (ws)
               (draw-tetris ws))]))
