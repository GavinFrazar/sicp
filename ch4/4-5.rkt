#lang sicp

(#%require "ddcore.rkt"
           (only racket/base
                 provide prefix-out))
(provide (prefix-out 4-5: cond->if))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (if (cond-recipient-clause? (cond-actions first))
                (make-recipient-if (cond-predicate first)
                                   (cond-recipient-func first)
                                   (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (cond-recipient-clause? clause)
  (and (pair? clause)
       (eq? (car clause) '=>)
       (pair? (cdr clause))
       (null? (cddr clause))))

(define (cond-recipient-func clause)
  (caddr clause))

(define (make-recipient-if predicate recipient-func alternative)
  (make-if predicate
           (list recipient-func predicate)
           alternative))

