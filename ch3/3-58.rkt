#lang sicp
(#%require "streams.rkt")

;; This procedure produces the floating-point quotient of num and den
;; as an infinite stream of digits
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den)
           den
           radix)))
