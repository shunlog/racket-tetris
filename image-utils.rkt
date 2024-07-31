#lang racket/base

(require 2htdp/image)

(require racket/contract)
(provide
 (contract-out
  [alpha-blend (-> color? color? number? color?)]
  [darker (->* (color?) (number?) color?)]))


;; Color Color (0 <= Float <= 1) -> Color
;; Alpha blend two colors (mix them)
;; leaving the alpha of the first color intact
(define (alpha-blend c1 c2 [alpha 0.5])
  (define (blend-components r1 r2)
    (inexact->exact (round (+ r1 (* alpha (- r2 r1))))))
  (make-color (blend-components (color-red c1) (color-red c2))
              (blend-components (color-green c1) (color-green c2))
              (blend-components (color-blue c1) (color-blue c2))
              (color-alpha c1)))

;; Color -> Color
;; Make color darker
(define (darker color [alpha 0.5])
  (alpha-blend color (make-color 0 0 0) alpha))
