#lang sicp
(#%require "streams.rkt")

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

;; Test
(define test (partial-sums integers))
