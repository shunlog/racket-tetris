#lang racket/base

;; This module creates a GUI frame for playing Tetris.

;; Time is measured using (millis),
;; which returns the monotonic, inexact, rounded number of ms.

;; -------------------------------
;; Constants


(define FPS 60)
(define FRAME-LABEL "World")
(define ROWS 20)
(define COLS 10)
(define GARBAGE-ROWS 0)
(define QUEUE-SIZE 5)

(define TIMER-INTERVAL (inexact->exact (round (/ 1000 FPS))))

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

(define (make-new-tetris)
  (new-tetris (millis)
              #:tetrion (~> (new-tetrion #:rows ROWS #:cols COLS #:queue-size QUEUE-SIZE)
                            (tetrion-add-garbage GARBAGE-ROWS))))



;; Converts number to string, and pads it with 0's
(define (num-pad-left n width)
  (define num-str (number->string n))
  (define padding (max 0 (- width (string-length num-str))))
  (string-append (make-string padding #\0) num-str))


;; Convert milliseconds to timer string, like "00:01:684"
(define (millis->string total-ms)
  (define ms (remainder total-ms 1000))
  (define s (remainder (quotient total-ms 1000) 60))
  (define m (quotient total-ms (* 1000 60)))
  (format "~a:~a:~a" (num-pad-left m 2) (num-pad-left s 2) (num-pad-left ms 3)))


;; -------------------------------
;; State variables

;; The Tetris state will be mutated for simplicity
(define tetris (make-new-tetris))
(define running? #f)


;; The (millis) when the game started
(define ms-start 0)


; -------------------------------
; Implementation



;; Tetris Key-event -> Tetris
;; Get the new state of the global tetris on key event
(define (tetris-on-event tetris key-ev)  
  (case (send key-ev get-key-code)
     [(left) (tetris-left-pressed tetris (millis))]
     [(right) (tetris-right-pressed tetris (millis))]
     [(up #\x) (tetris-rotate-cw tetris (millis))]
     [(#\z) (tetris-rotate-ccw tetris (millis))]
     [(#\a) (tetris-rotate-180 tetris (millis))]
     [(#\space) (tetris-hard-drop tetris (millis))]
     [(#\c) (tetris-hold tetris (millis))]
     [(down) (tetris-soft-drop-pressed tetris (millis))]
     [(release)
      (case (send key-ev get-key-release-code)
        [(left) (tetris-left-released tetris (millis))]
        [(right) (tetris-right-released tetris (millis))]
        [(down) (tetris-soft-drop-released tetris (millis))]
        [else tetris])]
     [else tetris]))


;; Override the frame%'s event handler,
;; which by default consumes some char events, including the arrow keys.
;; Make the frame ignore auto-key / autofire
(define tetris-frame%
  (class frame%
    (init) (super-new)

    (define keys-state-hash (make-hash))

    (define/override (on-subwindow-char receiver event)
      (define release-kc (send event get-key-code))
      (define pressed? (not (equal? 'release release-kc)))
      (define key-code
        (if pressed? release-kc (send event get-key-release-code)))
      (define was-pressed? (hash-ref keys-state-hash key-code #f))

      (unless ;; filter autofire
          (and was-pressed? pressed?)
        (tetris-frame-on-event event))
      
      (hash-set! keys-state-hash key-code pressed?)
      #f  ;; return #f to pass the event further
      )))


;; Key-event -> void
;; Update the tetris on key press/release
(define (tetris-frame-on-event key-ev)
  (cond
    [(eq? 'f4 (send key-ev get-key-code))
     (restart-game)]
    [(not running?) (void)]
    [else (with-handlers
            ([exn:fail:tetris:gameover? (λ (_) (game-over))])
            (set! tetris (tetris-on-event tetris key-ev)))]))


;; void -> void
;; Update the tetris on a clock tick (called by timer)
(define (on-tick)
  ;; (yield)
  (set! tetris (tetris-on-tick tetris (millis)))
  (send lines-cleared-msg set-label (number->string (tetrion-cleared (tetris-tn tetris))))
  (send timer-msg set-label (millis->string (- (millis) ms-start)))
  (update-canvases))


(define (update-canvases)
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
  (send queue-canvas refresh-now)
  (send hold-piece-canvas refresh-now))


;; ----------------------------
;; GUI

;; Width and Height of canvases:

;; Main tetris canvas:
(define-values (tw th)
  (~> tetris tetris-tn tetrion-playfield playfield-pict
      ((λ (pic) (values (pict-width pic) (pict-height pic))))))

;; Queue canvas
(define-values (qw qh)
  (~> tetris tetris-tn tetrion-queue queue-pict
      ((λ (pic) (values (pict-width pic) (pict-height pic))))))

;; Hold canvas
(define-values (hold-w hold-h)
  (~> tetris tetris-tn tetrion-on-hold hold-piece-pict
      ((λ (pic) (values (pict-width pic) (pict-height pic))))))

(define frame
  (new (class tetris-frame%
         (super-new)
         (define/augment (on-close)
           (exit)))
       [label FRAME-LABEL]
       [width (+ tw qw hold-w)]
       [height (max th qh hold-h)]))

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


(define hold-piece-canvas
  (new canvas%
       [parent horiz-pane]
       [style '(border)]
       [min-width hold-w]
       [min-height hold-h]
       [stretchable-width #f]
       [stretchable-height #f]
       [paint-callback
        (lambda (canvas dc)
          (define hold-shape (~> tetris tetris-tn tetrion-on-hold))
          (define pic (hold-piece-pict hold-shape))
          (draw-pict pic dc 0 0))]))

(define main-vert-pane
  (new vertical-pane%
       [parent horiz-pane]
       [spacing 20]
       [alignment '(center top)]))

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

(define tetris-canvas
  (new canvas%
       [parent main-vert-pane]
       [style '(border)]
       [min-width tw]
       [min-height th]
       [stretchable-width #f]
       [stretchable-height #f]
       [paint-callback
        (lambda (canvas dc)
          (define plf (~> tetris tetris-tn (tetrion-playfield #t)))
          (define pic (playfield-pict plf))
          (draw-pict pic dc 0 0))]))


(define game-over-msg
  (new message%
       [label "Game over! Press F4 to restart."]
       [parent main-vert-pane]))
(send game-over-msg show #f)


(define lines-cleared-msg
  (new message%
       [label "0"]
       [parent main-vert-pane]))


(define timer-msg
  (new message%
       [label "0"]
       [parent main-vert-pane]))


;; Make sure the timer doesn't start automatically
(define timer
  (new timer%
       [notify-callback on-tick]))


(define (game-over)
  (send timer stop)
  (set! running? #f)
  (send game-over-msg show #t)
  (update-canvases))


(define (restart-game)
  (set! tetris (make-new-tetris))
  (send timer start TIMER-INTERVAL)
  (set! running? #t)
  (set! ms-start (millis))
  (send game-over-msg show #f))


(send frame show #t)

;; Start the game at this point
(restart-game)
