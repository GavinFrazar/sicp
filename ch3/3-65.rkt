#lang sicp
(#%require "streams.rkt")
(#%require "3-59.rkt")

(define ln2-summands
  (stream-map
   (lambda (x)
     (let ((y (if (= (remainder x 2) 0)
                  (- x)
                  x)))
       (/ 1. y)))
   integers))

(define ln2
  (partial-sums ln2-summands))

(define ln2-faster
  (euler-transform ln2))

(define ln2-fastest
  (accelerated-sequence euler-transform
                        ln2))
