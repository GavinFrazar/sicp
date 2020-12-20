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

