#lang racket/base

(require racket/contract)
(require 2htdp/universe)


;; The `big-bang` function provided by 2htdp/universe
;; calls its on-key clause on every key-down event,
;; which includes the autofire events sent by the OS.
;; Unfortunately `big-bang` doesn't provide a way to ignore the autofires,
;; so this function is a helper for that.
;; It gives you functions to wrap the on-key and on-release functions,
;; so that they're only called only on events
;; transitioning from a pressed to released state, and viceversa,
;; effectively ignoring the autofire events.

;; Example usage:
;;   (define-values (on-key-filtered on-release-filtered)
;;     (filtered-autofire))
;;
;;  (define (my-on-key-filtered ws k)
;;    (on-key-filtered ws k (λ () (my-on-key ws k))))
;; 
;; Inside big-bang:
;;    [on-key (λ (ws k) (my-on-key-filtered ws k))]

(provide
 (contract-out
  [filtered-autofire (-> (values
                          (-> any/c key-event? (-> any) any/c)
                          (-> any/c key-event? (-> any) any/c)))] ))


;; Returns two functions, each takes
;; - world-state
;; - key
;; - func: function to call when the key wasn't an autofire
;;
;; When the first returned function is called,
;; it will only return the result of calling `(func)`
;; if the key wasn't already pressed.
;; Conversely, the second function will only return the result of `(func)`
;; if the key wasn't already released
(define (filtered-autofire)
  (define key-pressed-hash (make-hash))
  (define (on-key-filtered ws k f)
    (define pressed? (hash-ref key-pressed-hash k #f))
    (cond
      [pressed? ws]
      [else (hash-set! key-pressed-hash k #t) (f)]))
  (define (on-release-filtered ws k f)
    ;; Although pressed? will never be false in normal conditions,
    ;; I'll handle that just in case
    (define pressed? (hash-ref key-pressed-hash k #f))
    (cond
      [pressed? (hash-set! key-pressed-hash k #f) (f)]
      [else ws]))
  (values on-key-filtered on-release-filtered))
