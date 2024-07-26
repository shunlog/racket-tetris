#lang racket/base

(require threading)
(require 2htdp/universe)

(require "playfield.rkt")
(require "block.rkt")
(require "frozen-tetris.rkt")
(require "draw.rkt")


(big-bang (new-frozen-tetris)
          [to-draw draw-frozen-tetris]
          [on-key
           (λ (ft k)
             (cond
               [(key=? k "down")
                (with-handlers ([exn:fail? (λ (e) ft)])
                  (frozen-tetris-drop ft))]
               [(key=? k "left")
                (with-handlers ([exn:fail? (λ (e) ft)])
                  (frozen-tetris-left ft))]
               [(key=? k "right")
                (with-handlers ([exn:fail? (λ (e) ft)])
                  (frozen-tetris-right ft))]
               [(key=? k "l")
                (frozen-tetris-lock ft)]
               [else ft]))])
