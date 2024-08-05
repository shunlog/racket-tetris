#lang racket/base


;; This module creates a GUI frame for playing Tetris.


;; -------------------------------
;; Constants


(define FPS 50)
(define FRAME-LABEL "World")


; -------------------------------
; Requires


(require threading)
(require racket/gui)
(require "tetris.rkt")
(require "draw.rkt")
(require "tetrion.rkt")


; -------------------------------
; Helpers


;; -> Natural
;; Returns the number of milliseconds since an unspecified starting time.
;; Not sensitive to system clock adjustements,
;; which is useful for this case since we need only de delta time.
;; However it doesn't represent the real time.
(define (millis)
  (~> (current-inexact-monotonic-milliseconds)
      floor
      inexact->exact))


;; -------------------------------
;; State variables


;; The Tetris state will be mutated for simplicity
(define tetris (new-tetris (millis)))


; -------------------------------
; Implementation


;; Override the frame%'s event handler,
;; which by default consumes some char events, including the arrow keys
(define tetris-frame%
  (class frame%
    (init) (super-new)
    (define/override (on-subwindow-char receiver event)
      (on-tetris-event event)
      #f  ;; pass the event further
      )))


;; Key-event -> void
;; Update the tetris on key press/release
(define (on-tetris-event key-ev)
  (define new-tetris
    (case (send key-ev get-key-code)
      [(left) (tetris-left-pressed tetris (millis))]
      [(right) (tetris-right-pressed tetris (millis))]
      [(up #\x) (tetris-rotate-cw tetris (millis))]
      [(#\z) (tetris-rotate-ccw tetris (millis))]
      [(#\a) (tetris-rotate-180 tetris (millis))]
      [(#\space) (tetris-hard-drop tetris (millis))]
      [(down) (tetris-soft-drop-pressed tetris (millis))]
      [(release)
       (case (send key-ev get-key-release-code)
         [(left) (tetris-left-released tetris (millis))]
         [(right) (tetris-right-released tetris (millis))]
         [(down) (tetris-soft-drop-released tetris (millis))])]))
  (if (void? new-tetris)
      (void)
      (set! tetris new-tetris)))


;; void -> void
;; Update the tetris on a clock tick (called by timer)
(define (on-tetris-tick)
  (set! tetris (tetris-on-tick tetris (millis)))
  ;; refresh-now is different from refresh in that it controls the flushing to the screen,
  ;; rather than letting the system decide when to flush.
  ;; It basically does this:
  ;; 1. suspend-flush
  ;; 2. (send canvas on-paint), which calls `paint-callback` by default
  ;; 3. resume-flush
  ;; 4. flush
  ;; This is more performant for animations with high FPS
  ;; because the frame isn't drawn to the screen until the frame buffer was completed,
  ;; so there are less calls to Xorg.
  ;; For more info, see docs on gui/Windowing/1.7 Animation in Canvases
  (send tetris-canvas refresh-now))


;; ----------------------------
;; GUI


(define-values (w h) (playfield-canvas-size (tetrion-playfield (tetris-tn tetris))))
(define frame
  (new tetris-frame%
       [label FRAME-LABEL]
       [width w]
       [height h]))


(define tetris-canvas
  (new canvas%
       [parent frame]
       [style '(border)]
       [paint-callback
        (lambda (canvas dc)
          (draw-playfield (~> tetris tetris-tn tetrion-playfield) dc))]))


(new timer%
     [notify-callback on-tetris-tick]
     [interval (inexact->exact (round (/ 1000 FPS)))])


(send frame show #t)

