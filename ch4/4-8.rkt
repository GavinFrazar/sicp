#lang sicp

(#%require "ddcore.rkt"
           (only racket/base
                 module+)
           rackunit)
(#%provide let->combination)

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination (cdr exp))
      (let ([bindings (let-bindings exp)])
        (cons (make-lambda (binding-parameters bindings) (let-body exp))
              (binding-expressions bindings)))))

(define (named-let->combination exp)
  (let ([named-let-id (named-let-identifier exp)]
        [bindings (let-bindings exp)])
    (begin
      (make-begin
       (list (list 'define named-let-id (make-lambda
                                         (binding-parameters bindings)
                                         (let-body exp)))
             (cons named-let-id (binding-expressions bindings)))))))

(define (named-let-identifier exp)
  (car exp))

(define (let-bindings exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (binding-parameters bindings)
  (map car bindings))

(define (binding-expressions bindings)
  (map cadr bindings))

(define (named-let? exp)
  (symbol? (cadr exp)))

(module+ test
  (define bindings '((n 5)))
  (define body '((if (= n 0)
                    n
                    (+ n (sum (- n 1))))))
  (define test-exp (cons 'let
                         (cons 'sum
                               (cons bindings body))))
  (check-equal? (let-bindings (cdr test-exp)) bindings)
  (check-equal? (let-body (cdr test-exp)) body)
  (check-equal? (binding-parameters bindings) '(n))
  (check-equal? (binding-expressions bindings) '(5))
  (check-equal? (let->combination test-exp)
                `(begin (define sum
                          (lambda (n)
                            (if (= n 0)
                                n
                                (+ n (sum (- n 1))))))
                        (sum 5))))

