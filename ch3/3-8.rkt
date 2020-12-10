#lang racket

(define f
  (let ((called #f))
    (λ (x) (if called
               0
               (begin (set! called #t)
                      x)))))
