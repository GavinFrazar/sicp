#lang sicp

(#%require "ddcore.rkt"
           "tests.rkt")

(define (eval-and exp env)
  (define (and-eval-loop exps last-val)
    (if (null? exps)
        last-val
        (let ([next-val (eval (car exps) env)])
          (if (true? next-val)
              (and-eval-loop (cdr exps) next-val)
              'false))))
  (and-eval-loop (and-operands exp) 'true))

(define (and-operands exp)
  (cdr exp))

(define (eval-or exp env)
  (define (or-eval-loop exps)
    (if (null? exps)
        'false
        (let ([next-val (eval (car exps) env)])
          (if (true? next-val)
              next-val
              (or-eval-loop (cdr exps))))))
  (or-eval-loop (or-operands exp)))

(define (or-operands exp)
  (cdr exp))


(install-special-form `(and ,eval-and))
(install-special-form `(or ,eval-or))

(test-eval (and) => 'true)
(test-eval (and 1) => 1)
(test-eval (and 1 2) => 2)
(test-eval (and false) => 'false)
(test-eval (and 1 false 2) => 'false)

(test-eval (or) => 'false)
(test-eval (or 1) => 1)
(test-eval (or 1 2) => 1)
(test-eval (or false 1) => 1)
(test-eval (or 1 false) => 1)

