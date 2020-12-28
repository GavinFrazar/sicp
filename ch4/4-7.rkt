#lang sicp

(#%require "ddcore.rkt"
           (only racket/base
                 provide prefix-out module+)
           rackunit)
(provide (prefix-out 4-7: let*->nested-lets))

(define (let*->nested-lets exp)
  (let*-expand (let*-parameters exp) (let*-body exp)))

(define (let*-expand parameters body)
  (if (null? parameters)
      (error "let* -- no parameters specified")
      (if (null? (cdr parameters))
          (cons 'let
                (cons (list (car parameters))
                      body))
          (list 'let
                (list (car parameters))
                (let*-expand (cdr parameters) body)))))

(define (let*-parameters exp)
  (cadr exp))

(define (let*-body exp)
  (cddr exp))

(module+ test
  (define params '((a 1)
                   (b (+ a 1))))
  (define body '((+ a b)))
  (define test-exp (cons 'let* (cons params body)))
  (check-equal? (let*-body test-exp)
                body)
  (check-equal? (let*-parameters test-exp)
                params)
  (check-equal? (let*-expand params body)
                '(let ((a 1))
                   (let ((b (+ a 1)))
                     (+ a b)))))
