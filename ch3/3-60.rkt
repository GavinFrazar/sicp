#lang sicp
(#%require "streams.rkt")
(#%require "3-59.rkt")

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (mul-series (stream-cdr s1)
                                        s2)
                            (scale-stream (stream-cdr s2)
                                          (stream-car s1)))))

(define sine**2 (mul-series sine-series
                            sine-series))
(define cos**2 (mul-series cosine-series
                           cosine-series))

(define one (add-streams sine**2 cos**2))

(#%provide (all-defined))
