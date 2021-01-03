#lang sicp

(#%require "ddcore.rkt"
           (only racket/base
                 module+)
           rackunit)
(#%provide while->combination)

(define (while->combination exp)
  `(let while-loop ()
     (if ,(while-cond exp)
         (begin
           ,(while-body exp)
           (while-loop))
         false)))

(define (while-cond exp)
  (cadr exp))

(define (while-body exp)
  (caddr exp))

(module+ test
  (define cond-exp '(> x 0))
  (define body '(set! x (- x 1)))
  (define test-exp `(while ,cond-exp ,body))
  (check-equal? (while-cond test-exp)
                cond-exp)
  (check-equal? (while-body test-exp)
                body)
  (check-equal? (while->combination test-exp)
                `(let while-loop ()
                   (if (> x 0)
                       (begin
                         (set! x (- x 1))
                         (while-loop))
                       false))))
