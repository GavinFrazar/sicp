#lang sicp
(#%require "streams.rkt")

(define (stream-limit s tolerance)
  (let ((a (stream-car s))
        (b (stream-car (stream-cdr s))))
    (if (< (abs (- a b)) tolerance)
        b
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
