#lang racket/base

(require "tetris-logic.rkt")
(require 2htdp/universe)

; Tetris, Key -> Tetris
(define (tetris-on-key t k)
  (cond
    [(key=? k " ") (tetris-soft-drop t)]
    [(key=? k "left") (tetris-move-piece t 'left)]
    [(key=? k "right") (tetris-move-piece t 'right)]
    [(or (key=? k "up") (key=? k "x")) (tetris-rotate-piece t 'cw)]
    [(key=? k "z") (tetris-rotate-piece t 'ccw)]
    ;; [(key=? k "a")
    ;;  (make-tetris (try-move-piece 'rotate-180 piece plf) plf)]
    [else t]))


(define (main tick-durn)
  (big-bang (tetris-init)
            [on-tick tetris-on-tick tick-durn]
            [to-draw draw-tetris]
            [on-key tetris-on-key]))
