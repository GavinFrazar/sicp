#lang sicp

(#%require "ddcore.rkt"
           "tests.rkt")

(#%provide eval-and
           eval-or)

(define (eval-and exp env)
  (define (and-eval-loop exps last-val)
    (if (null? exps)
        last-val
        (let ([next-val (eval (car exps) env)])
          (if (true? next-val)
              (and-eval-loop (cdr exps) next-val)
              false))))
  (and-eval-loop (and-operands exp) true))

(define (and-operands exp)
  (cdr exp))

(define (eval-or exp env)
  (define (or-eval-loop exps)
    (if (null? exps)
        false
        (let ([next-val (eval (car exps) env)])
          (if (true? next-val)
              next-val
              (or-eval-loop (cdr exps))))))
  (or-eval-loop (or-operands exp)))

(define (or-operands exp)
  (cdr exp))

(install-special-form `(and ,eval-and))
(install-special-form `(or ,eval-or))

(define-eval-test-suite
  and-or-tests
  ("and w/o values returns false"
   (and) => true)
  ("and with a single truthy value returns its value"
   (and 1) => 1)
  ("and with all truthy values returns the last value"
   (and 1 2) => 2)
  ("and with a false arg returns false"
   (and false) => false)
  ((and 1 false 2) => false)
  ((or) => false)
  ((or 1) => 1)
  ((or 1 2) => 1)
  ((or false 1) => 1)
  ((or 1 false) => 1)
  ("nested predicate evaluates correctly"
   (or (and false) 42) => 42))

(run-tests and-or-tests)

