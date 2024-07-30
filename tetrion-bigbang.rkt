#lang racket/base

(require threading)
(require 2htdp/universe)

(require "playfield.rkt")
(require "tetrion.rkt")
(require "draw.rkt")


(big-bang (new-tetrion)
          [to-draw draw-tetrion]
          [on-key
           (λ (tn k)
             (cond
               [(key=? k "down")
                (with-handlers ([exn:fail? (λ (e) tn)])
                  (tetrion-drop tn))]
               [(key=? k "left")
                (with-handlers ([exn:fail? (λ (e) tn)])
                  (tetrion-left tn))]
               [(key=? k "right")
                (with-handlers ([exn:fail? (λ (e) tn)])
                  (tetrion-right tn))]
               [(key=? k "l")
                (tetrion-lock tn)]
               [else tn]))])
