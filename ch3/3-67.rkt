#lang sicp
(#%require "streams.rkt")

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t)) ; rest of this row
   (interleave
    (interleave
     (stream-map (lambda (x)
                   (list x (stream-car t)))
                 (stream-cdr s))
     (stream-map (lambda (x)
                   (list (stream-car s) x))
                 (stream-cdr t)))
    (pairs (stream-cdr s)
           (stream-cdr t)))))
