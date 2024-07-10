#lang racket/base

(require "tetris-logic.rkt")
(require "tetris-draw.rkt")
(require 2htdp/universe)

(provide main)

; Tetris, Key -> Tetris
(define (tetris-on-key t k)
  (cond
    [(key=? k " ") (tetris-move t 'soft-drop)]
    [(key=? k "left") (tetris-move t 'left)]
    [(key=? k "right") (tetris-move t 'right)]
    [(or (key=? k "up") (key=? k "x")) (tetris-move t 'cw)]
    [(key=? k "z") (tetris-move t 'ccw)]
    [(key=? k "a") (tetris-move t 180)]
    [else t]))


(define (main tick-durn)
  (big-bang (tetris-init)
            [on-tick tetris-on-tick tick-durn]
            [to-draw draw-any-blocks]
            [on-key tetris-on-key]
            [on-release (lambda (t ev) (begin (writeln ev) t))]))
