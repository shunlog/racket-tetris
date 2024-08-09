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
(require pict)
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
;; which by default consumes some char events, including the arrow keys.
;; Make the frame ignore auto-key / autofire
(define tetris-frame%
  (class frame%
    (init) (super-new)

    (define keys-state-hash (make-hash))

    (define/override (on-subwindow-char receiver event)
      (define kc (send event get-key-code))
      (define pressed? (not (equal? 'release kc)))
      (define key-code
        (if pressed? kc (send event get-key-release-code)))
      (define was-pressed? (hash-ref keys-state-hash key-code #f))

      ;; only pass event to tetris if not an autofire
      (unless (and was-pressed? pressed?)
        (on-tetris-event event))

      (hash-set! keys-state-hash key-code pressed?)
      #f  ;; return #f to pass the event further
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
  (send tetris-canvas refresh-now)
  (send queue-canvas refresh-now))


;; ----------------------------
;; GUI

(define-values (tw th)
  (playfield-canvas-size (tetrion-playfield (tetris-tn tetris))))

(define-values (qw qh)
  (~> tetris
      tetris-tn
      tetrion-queue
      queue-pict
      ((λ (pic) (values (pict-width pic) (pict-height pic))))))

(define frame
  (new tetris-frame%
       [label FRAME-LABEL]
       [width (+ tw qw)]
       [height (max th qh)]))

(define margin-container
  (new pane%
       [parent frame]
       [border 20]
       [alignment '(center top)]))

(define horiz-pane
  (new horizontal-pane%
       [parent margin-container]
       [spacing 20]
       [alignment '(center top)]))


(define tetris-canvas
  (new canvas%
       [parent horiz-pane]
       [style '(border)]
       [min-width tw]
       [min-height th]
       [stretchable-width #f]
       [stretchable-height #f]
       [paint-callback
        (lambda (canvas dc)
          (draw-playfield (~> tetris tetris-tn (tetrion-playfield #t)) dc))]))


(define queue-canvas
  (new canvas%
       [parent horiz-pane]
       [style '(border)]
       ;; Hard-coding the size to the first pict of the preview
       ;; might fail in case larger shapes will appear later.
       ;; Ideally the size should fit all the shapes in the bag.
       [min-width qw]
       [min-height qh]
       [stretchable-width #f]
       [stretchable-height #f]
       [paint-callback
        (lambda (canvas dc)
          (define queue (~> tetris tetris-tn tetrion-queue))
          (define pic (queue-pict queue))
          (draw-pict pic dc 0 0))]))

(new timer%
     [notify-callback on-tetris-tick]
     [interval (inexact->exact (round (/ 1000 FPS)))])


(send frame show #t)
