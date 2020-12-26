#lang sicp

(#%require (only racket/base
                 require
                 prefix-in
                 format))

(require rackunit
         rackunit/text-ui
         (prefix-in core:
                    "ddcore.rkt"))

(#%provide set-eval!
           set-env!
           test-eval
           test-eval/diff
           define-test-suite/tester
           define-eval-test-suite
           define-eval-test/diff-suite
           all-eval-tests
           begin-tests
           primitive-diff-tests
           run-tests)

(define test-evaluator core:eval)
(define test-env core:the-global-environment)
(define (set-eval! new-eval)
  (set! test-evaluator new-eval))
(define (set-env! new-env)
  (set! test-env new-env))

;; macro helpers to reduce test verbosity
(define-syntax test-eval
  (syntax-rules (=>)
    [(_ expr => expected)
     (check-not-exn
      (lambda ()
        (check-equal?
         (test-evaluator 'expr test-env)
         expected)))]
    [(_ test-case-name expr => expected)
     (test-case (format "\"~a\"" test-case-name)
       (check-not-exn
        (lambda ()
          (check-equal?
           (test-evaluator 'expr test-env)
           expected))))]))

(define-syntax test-eval/diff
  (syntax-rules ()
    [(_ expr)
     (test-eval expr => (eval 'expr (scheme-report-environment 5)))]))

(define-syntax define-test-suite/tester
  (syntax-rules (=>)
    [(_ tester suite-name (test-case-name ... test-expr => test-expected) ...)
     (define-test-suite suite-name
       (tester test-case-name ... test-expr => test-expected) ...)]))

(define-syntax define-eval-test-suite
  (syntax-rules ()
    [(_ suite-name tests ...)
     (define-test-suite/tester test-eval suite-name tests ...)]))

(define-syntax define-eval-test/diff-suite
  (syntax-rules ()
    [(_ suite-name tests ...)
     (define-test-suite suite-name
       (test-eval/diff tests) ...)]))

(define-eval-test-suite
  begin-tests
  ("evals to expression value"
   (begin 1) => 1)
  ("evals to last expression's value"
   (begin 1 2 3) => 3)
  ("complex-1"
   (begin
     (define (fact n)
       (if (= n 0)
           1
           (* n (fact (- n 1)))))
     (fact 10))
   => 3628800))

(define-eval-test-suite
  cond-tests
  ("cond evaluates first true consequent"
   (cond [true 1]
         [true 2])
   => 1)
  ("cond does not evaluate false consequent"
   (cond [false (/ 1 0)]
         [true 2])
   => 2)
  ("cond does not evaluate alternative"
   (cond [true 1]
         [false (/ 1 0)])
   => 1)
  ("cond evaluates else clause"
   (cond [false 1]
         [else 2])
   => 2))

(define-eval-test-suite
  lambda-tests
  ("lambda captures its environment"
   (((lambda (a) (lambda () a)) 42))
   => 42)
  ("lambda shadows variables"
   (((lambda (a) (lambda (a) a)) 1) 2)
   => 2))

(define-eval-test-suite
  quote-tests
  ("quoted expression is not evaluated"
   (quote (/ 1 0))
   => '(/ 1 0))
  ("quote quote"
   (quote (quote a))
   => '(quote a))
  ("short-hand quote"
   'a => 'a))

(define-eval-test/diff-suite
  primitive-diff-tests
  (cons 1 2)
  (car (cons 1 2))
  (cdr (cons 1 2))
  (+ 1 1)
  (- 1 1)
  (* 2 3)
  (= 1 1)
  (= 1 2)
  (eq? (cons 1 2) (cons 1 2))
  (eq? 1 1))

;; TODO: use foldts-test-suite to setup/teardown the env around each suite
(define-test-suite all-eval-tests
  begin-tests
  cond-tests
  lambda-tests
  quote-tests
  primitive-diff-tests)

;; (test/eval test-evaluator test-env) ;; test my testing :D

