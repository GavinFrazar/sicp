#lang sicp

(#%require (only racket/base
                 require
                 prefix-in
                 format))

(require rackunit
         rackunit/text-ui
         (prefix-in core:
                    "ddcore.rkt"))

(define test-evaluator core:eval)
(define test-env core:the-global-environment)

;; macro helpers to reduce test verbosity
(define-syntax test-eval
  (syntax-rules (=>)
    [(_ expr => expected)
     (check-equal?
      (test-evaluator 'expr test-env)
      expected)]
    [(_ test-case-name expr => expected)
     (test-case (format "\"~a\"" test-case-name)
       (check-equal?
        (test-evaluator 'expr test-env)
        expected))]))

(define-syntax test-suite/tester
  (syntax-rules (=>)
    [(_ tester suite-name (test-case-name ... test-expr => test-expected) ...)
     (test-suite suite-name
       (tester test-case-name ... test-expr => test-expected) ...)]))

(define-syntax eval-test-suite
  (syntax-rules ()
    [(_ suite-name tests ...)
     (test-suite/tester test-eval suite-name tests ...)]))

(define (test/eval evaluator env)
  (set! test-evaluator evaluator)
  (set! test-env env)

  ;; TODO: use foldts-test-suite to setup/teardown the env around each suite
  (define-test-suite all-eval-tests
    (eval-test-suite "primitive-expression"
      (1 => 1)
      ("abc" => "abc"))

    (eval-test-suite "primitive-procedure"
      ((cons 1 2) => (cons 1 2))
      ((car (cons 1 2)) => 1)
      ((cdr (cons 1 2)) => 2)
      ((+ 1 1) => 2)
      ((- 1 1) => 0)
      ((* 2 3) => 6)
      ((= 1 1) => #t)
      ((= 1 2) => #f)
      ((eq? (cons 1 2) (cons 1 2)) => #f)
      ((eq? 1 1) => #t))

    (eval-test-suite "begin"
      ((begin 1) => 1)
      ("begin form evals to last expression's value"
       (begin 1 2 3) => 3)
      ("begin/complex 1"
       (begin
         (define (fact n)
           (if (= n 0)
               1
               (* n (fact (- n 1)))))
         (fact 10))
       => 3628800)))
  (run-tests all-eval-tests))

;;(test/eval core:eval core:the-global-environment) ;; test my testing :D

(#%provide (all-defined))
