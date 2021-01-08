#lang sicp
(#%require racket/include
           "ddcore.rkt"
           (only racket/base
                 module+)
           rackunit)

(include "ddcore-head.rkt")
(#%provide (all-defined))

;; Solution code
(set! make-frame
      (lambda (variables values)
        (map cons variables values)))

(set! frame-variables
      (lambda (frame) (map car frame)))

(set! frame-values
      (lambda (frame) (map cdr frame)))

(set! add-binding-to-frame!
      (lambda (var val frame)
        (set-cdr! frame (cons (car frame) (cdr frame)))
        (set-car! frame (cons var val))))

(include "ddcore-init.rkt")

(module+ test
  (define env-x (extend-environment '(x y z)
                                    '(1 2 3)
                                    the-global-environment))
  (define frame-x (first-frame env-x))
  (define frame-vars '(x y z))
  (define frame-vals '(1 2 3))
  (define a-var 'a)
  (define a-val 42)
  (check-equal? frame-vars (frame-variables frame-x))
  (check-equal? frame-vals (frame-values frame-x))
  (add-binding-to-frame! a-var a-val frame-x)
  (check-equal? a-val (lookup-variable-value a-var env-x)))

