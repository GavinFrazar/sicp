#lang sicp
(#%require "streams.rkt")
(#%require "3-59.rkt")
(#%require "3-60.rkt")
(#%require "3-61.rkt")

(define (div-series s1 s2)
  (mul-series s1 (invert-series s2)))

(define tan-series (div-series sine-series cosine-series))
