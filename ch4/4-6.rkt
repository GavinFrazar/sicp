#lang sicp

(#%require "ddcore.rkt"
           (only racket/base
                 module+)
           rackunit)
(#%provide let->combination)

(define (let->combination exp)
  (let ([bindings (let-bindings exp)])
    (cons (make-lambda (binding-parameters bindings) (let-body exp))
          (binding-expressions bindings))))

(define (let-bindings exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (binding-parameters bindings)
  (map car bindings))

(define (binding-expressions bindings)
  (map cadr bindings))

(module+ test
  (define bindings '((a (+ x 1))
                     (b 42)))
  (define body '((+ a b)))
  (define test-exp (cons 'let (cons bindings body)))
  (check-equal? (let-bindings test-exp)
                bindings)
  (check-equal? (let-body test-exp)
                body)
  (check-equal? (binding-parameters bindings)
                '(a b))
  (check-equal? (binding-expressions bindings)
                '((+ x 1) 42))
  (check-equal? (let->combination test-exp)
                '((lambda (a b) (+ a b)) (+ x 1) 42)))
