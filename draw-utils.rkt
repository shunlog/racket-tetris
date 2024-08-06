#lang racket/base

(require racket/draw)
(require racket/class)

(require racket/contract)
(provide
 (contract-out
  [alpha-blend (-> (is-a?/c color%) (is-a?/c color%) number? (is-a?/c color%))]
  [darker (->* ((is-a?/c color%)) (number?) (is-a?/c color%))]
  [set-alpha (-> (is-a?/c color%) number? (is-a?/c color%))]))


;; Color Color (0 <= Float <= 1) -> Color
;; Alpha blend two colors (mix them)
;; leaving the alpha of the first color intact
(define (alpha-blend c1 c2 [alpha 0.5])
  (define (blend-components r1 r2)
    (inexact->exact (round (+ r1 (* alpha (- r2 r1))))))
  (make-object color%
               (blend-components (send c1 red) (send c2 red))
               (blend-components (send c1 green) (send c2 green))
               (blend-components (send c1 blue) (send c2 blue))
               (send c1 alpha)))

;; Color Interval[0, 1] -> Color
(define (set-alpha c alpha)
  (make-color (send c red) (send c green) (send c blue) alpha))

;; Color -> Color
;; Make color darker
(define (darker color [alpha 0.5])
  (alpha-blend color (make-color 0 0 0) alpha))
