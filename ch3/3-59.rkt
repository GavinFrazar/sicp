#lang sicp
(#%require "streams.rkt")
(#%require "3-54.rkt")

(define integer-reciprocals
  (stream-map (lambda (x) (/ 1 x)) integers))

(define (integrate-series s)
  (mul-streams integer-reciprocals s))

(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream
   1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream
   0 (integrate-series cosine-series)))

(#%provide (all-defined))
