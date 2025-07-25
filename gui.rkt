#lang racket/base

;; This module creates a GUI frame for playing Tetris.

;; Time is measured using (millis),
;; which returns the monotonic, inexact, rounded number of ms.

(require threading
         racket/gui
         pict
         mode-lambda
         mode-lambda/static
         mode-lambda/backend/gl
         "tetris.rkt"
         "draw.rkt"
         "tetrion.rkt"
         "playfield.rkt"
         "block.rkt"
         "shapes.rkt"
         lang/posn
         racket/fixnum)

;; -------------------------------
;; Constants

(define FPS 60)
(define FRAME-LABEL "World")

(define ROWS 20)
(define COLS 10)
(define VANISH-LINES 2)
(define ROWS-TOTAL (+ ROWS VANISH-LINES))

(define GARBAGE-ROWS 0)
(define QUEUE-SIZE 5)
(define LINES-CLEARED-GOAL 5)

(define LAYERS 8)            ; mode-lambda layers, unsafe limit is 256

;;; Playfield canvas size
(define W (* BLOCK-W COLS))
(define H (* BLOCK-W ROWS-TOTAL))


;; -------------------------------
;; State variables

;; The Tetris state will be mutated for simplicity
(define tetris null)
(define running? #f)
(define fps-last-ts 0)
(define current-fps 0)

;; The (millis) when the game started
(define ms-start 0)


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

(define (update-fps!)
  (define ms (millis))
  (set! current-fps (/ 1000.0 (- ms fps-last-ts)))
  (set! fps-last-ts ms))


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


;; Tetris Key-event -> Tetris
;; Get the new state of the global tetris on key event
(define (tetris-on-event t1 key-ev)
  (define ms (millis))
  (case (send key-ev get-key-code)
     [(left) (tetris-left-pressed t1 ms)]
     [(right) (tetris-right-pressed t1 ms)]
     [(up #\x) (tetris-rotate-cw t1 ms)]
     [(#\z) (tetris-rotate-ccw t1 ms)]
     [(#\a) (tetris-rotate-180 t1 ms)]
     [(#\space) (tetris-hard-drop t1 ms)]
     [(#\c) (tetris-hold t1 ms)]
     [(down) (tetris-soft-drop-pressed t1 ms)]
     [(#\s) (tetris-revert t1)]
     [(release)
      (case (send key-ev get-key-release-code)
        [(left) (tetris-left-released t1 ms)]
        [(right) (tetris-right-released t1 ms)]
        [(down) (tetris-soft-drop-released t1 ms)]
        [else t1])]
     [else t1]))



;;; ----------------------------
;;; Event handlers (input, update, draw)


;; Key-event -> void
;; Update the tetris on key press/release
(define (tetris-frame-on-event key-ev)
  (cond
    [(eq? 'f4 (send key-ev get-key-code))
     (restart-game)]
    [(not running?) (void)]
    [else (with-handlers
            ([exn:fail:tetris:gameover? (λ (msg) (game-over msg))])
            ;; run the on-tick-update before handling the user input.
            ;; this is the idealized scenario,
            ;; it is equivalent to running on-tick infinitely fast, repeatedly.
            ;; the only downside is that the player doesn't see the updates just as fast,
            ;; but we can compensate by delaying the on-tick ms a bit (e.g. 16ms).
            ;; However this is better because we make the compensation explicit
            ;; rather than relying on the clock speed
            (update)
            (set! tetris (tetris-on-event tetris key-ev))
            (draw)
            (when (<= LINES-CLEARED-GOAL (tetrion-cleared (tetris-tn tetris)))
              (raise-tetris-gameover "Cleared all the lines")))])
  (send lines-cleared-count-msg set-label (number->string (tetrion-cleared (tetris-tn tetris)))))


;; void -> void
;; Update the tetris on a clock tick (called by timer)
(define (update)
  (define old-t tetris)
  (set! tetris (tetris-on-tick tetris (millis)))
)


(define (draw)
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
  (update-fps!)
  (send tetris-canvas refresh-now)
  (send queue-canvas refresh-now)
  (send hold-piece-canvas refresh-now)
  (send frame set-label (format "FPS: ~v"
                                (number->string
                                 (/ (round (* 10 current-fps)) 10.0))))
  (send timer-msg set-label (millis->string (- (millis) ms-start)))
  )



;; ----------------------------
;; Set up mode-lambda sprites


(define db (make-sprite-db))

;;; Convert tile to symbol,
;;; Needed for the mode-lambda sprite database to index tiles
(define (tile->sym tile)
  (cond
    [(tile-garbage? tile) 'garbage]
    [(tile-ghost? tile)
     (string->symbol (string-append "ghost-" (symbol->string (tile-shape tile))))]
    [else
     (string->symbol (string-append "normal-" (symbol->string (tile-shape tile))))]))

;;; Generate picts for each possible tile
(for ([sn SHAPE-NAMES])
  (define tile (tile-normal sn))
  (define tilepict (tile-pict tile))
  (add-sprite!/value db (tile->sym tile) tilepict))
(for ([sn SHAPE-NAMES])
  (define tile (tile-ghost sn))
  (define tilepict (tile-pict tile))
  (add-sprite!/value db (tile->sym tile) tilepict))
(let ()
  (define tile TILE-GARBAGE)
  (define tilepict (tile-pict tile))
  (add-sprite!/value db (tile->sym tile) tilepict))

(define cdb (compile-sprite-db db))

;;; it should only be initialized once
(define ml-draw
  (stage-draw/dc cdb W H LAYERS))


;;; -----------------------------
;;; Mode-lambda state variables


;;; These lists will be mutated
(define static (list ))
(define dynamic (list ))

;;; This vector can be mutated as well, but that's advanced
(define ml-layers
  (make-vector LAYERS (layer (* W 0.5) (* H 0.5))))


(define gl-conf (new gl-config%))
(send gl-conf set-hires-mode #t)
(send gl-conf set-legacy? #f)

(define (update-dynamic!)
  (define plf (tetrion-playfield (tetris-tn tetris) #t))
  (define sprites
    (for/list ([blck (playfield-blocks plf)])
     (define col (posn-x (block-posn blck)))
     (define row (posn-y (block-posn blck)))
     (define x col)
     (define y (- ROWS-TOTAL row 1))
     (define cx (fx->fl (+ (* BLOCK-W x) (/ BLOCK-W 2))))
     (define cy (fx->fl (+ (* BLOCK-W y) (/ BLOCK-W 2))))
     (define sprite-id (sprite-idx cdb (tile->sym (block-tile blck))))
     (sprite cx cy sprite-id)))
  (set! dynamic sprites))



;; ----------------------------
;; GUI

;;; needed to get dimensions
(set! tetris (make-new-tetris))


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
       [width (+ W qw hold-w)]
       [height (max H qh hold-h)]))

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
       [spacing 10]
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
       [min-width W]
       [min-height H]
       [stretchable-width #f]
       [stretchable-height #f]
       [gl-config gl-conf]
       [style '(border no-autoclear gl)]
       [paint-callback
        (λ (c dc)
          (update-dynamic!)
          (define draw (ml-draw ml-layers static dynamic))
          (match/values (send c get-gl-client-size)
            [(w h) (draw w h dc)]))]))

(define timer-msg
  (new message%
       [label "[timer..........]"]
       [parent main-vert-pane]))

(define goal-msg
  (new message%
       [label (format "Sprint: Clear ~v lines" LINES-CLEARED-GOAL)]
       [parent main-vert-pane]))

(define lines-cleared-pane
  (new horizontal-pane%
       [parent main-vert-pane]
       [alignment '(center top)]))
(define lines-cleared-msg
  (new message%
       [label "Lines cleared:"]
       [parent lines-cleared-pane]))
(define lines-cleared-count-msg
  (new message%
       [label "0"]
       [parent lines-cleared-pane]))

(define game-over-msg
  (new message%
       [label "Game over! Press F4 to restart."]
       [parent main-vert-pane]))
(send game-over-msg show #f)



;;; ---------------------------------
;; Game loop

;;; More on game loops: https://gameprogrammingpatterns.com/game-loop.html

(define (game-loop)
  (let loop ()
    (define ms-start (millis))
    (when running?
      (update)
      (draw))
    (define ms-passed (- (millis) ms-start))
    (define ms-leftover (- (/ 1000.0 FPS) ms-passed))
    ;; Let GUI process any pending events even when late
    (define ms-sleep (max 1.0 ms-leftover))
    (sleep/yield (/ ms-sleep 1000.0))
    (loop)))


(define (game-over msg)
  (set! running? #f)
  (send game-over-msg show #t)
  (println msg)
  (draw))


(define (restart-game)
  (set! tetris (make-new-tetris))
  (set! running? #t)
  (set! ms-start (millis))
  (send game-over-msg show #f)
  (draw)
  )


(send frame show #t)
(restart-game)
(game-loop)
