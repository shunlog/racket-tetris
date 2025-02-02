#lang racket/base
(require racket/draw)
(require racket/class)

(provide
 GARBAGE-COLOR
 COLORS-HASH
 color%)

(define (make-color r g b) (make-object color% r g b))

(define GARBAGE-COLOR (make-color 156 154 154))

(define COLORS-HASH
  (hash 'L (make-color 255 128 0)
        'J (make-color 0 132 255)
        'S (make-color 0 217 51)
        'Z (make-color 245 7 7)
        'T (make-color 205 7 245)
        'I (make-color 0 247 255)
        'O (make-color 242 235 12)))
