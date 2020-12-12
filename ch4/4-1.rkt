#lang sicp

(#%require "core.rkt")

(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let* ((left (eval (first-operand exps) env))
             (right (list-of-values (rest-operands exps) env)))
        (cons left right))))

(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let* ((right (list-of-values (rest-operands exps) env))
             (left (eval (first-operand exps) env)))
        (cons left right))))

