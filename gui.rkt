#lang racket/base

(require threading)
(require racket/gui)
(require "tetris.rkt")
(require "draw2.rkt")
(require "tetrion.rkt")

(define (millis)
  (~> (current-inexact-monotonic-milliseconds)
      floor
      inexact->exact))

(define tetris (tetris-hard-drop (new-tetris (millis)) (millis)))


(define tetris-frame%
  (class frame%
    (init) (super-new)
    (define/override (on-subwindow-char receiver event)
      (on-tetris-event event)
      (super on-subwindow-char receiver event))))


(define-values (w h) (playfield-canvas-size (tetrion-playfield (tetris-tn tetris))))
(define frame (new tetris-frame%
                   [label "World"]
                   [width w]
                   [height h]))


(define tetris-canvas (new canvas%
      [parent frame]
      [style '(border)]
      [paint-callback
       (lambda (canvas dc)
         (draw-playfield (~> tetris tetris-tn tetrion-playfield) dc))]))

(define (on-tetris-event ev)  
  (case (list (send ev get-key-code) (send ev get-key-release-code))
    [((left press))
     (set! tetris (tetris-left-pressed tetris (millis)))]
    [((release left))
     (set! tetris (tetris-left-released tetris (millis)))]
    [((right press))
     (set! tetris (tetris-right-pressed tetris (millis)))]
    [((release right))
     (set! tetris (tetris-right-released tetris (millis)))]
    [((up press) (#\x press))
     (set! tetris (tetris-rotate-cw tetris (millis)))]
    [((#\z press))
     (set! tetris (tetris-rotate-ccw tetris (millis)))]
    [((#\a press))
     (set! tetris (tetris-rotate-180 tetris (millis)))]
    [((#\space press))
     (set! tetris (tetris-hard-drop tetris (millis)))]
    [((down press))
     (set! tetris (tetris-soft-drop-pressed tetris (millis)))]
    [((release down))
     (set! tetris (tetris-soft-drop-released tetris (millis)))]
    )
  (send tetris-canvas refresh-now))

(define (on-tetris-tick)
  (set! tetris (tetris-on-tick tetris (millis)))
  (send tetris-canvas refresh-now))

(define FPS 50)
(define timer (new timer%
                   [notify-callback on-tetris-tick]
                   [interval (inexact->exact (round (/ 1000 FPS)))]))

(send frame show #t)

