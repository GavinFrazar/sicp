#lang sicp

(#%require (only racket/base
                 require
                 prefix-in
                 only-in
                 format))

(require rackunit
         rackunit/text-ui
         (prefix-in core:
                    "ddcore.rkt")
         (prefix-in 4-4: (only-in "4-4.rkt"
                                  eval-and
                                  eval-or))
         (prefix-in 4-5: (only-in "4-5.rkt"
                                  cond->if))
         (prefix-in 4-6: (only-in "4-6.rkt"
                                  let->combination))
         (prefix-in 4-7: (only-in "4-7.rkt"
                                  let*->nested-lets)))

(#%provide set-eval!
           set-env!
           test-eval
           test-eval/diff
           define-test-suite/tester
           define-eval-test-suite
           define-eval-test/diff-suite
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
  ("evaluates first true consequent"
   (cond [true 1]
         [true 2])
   => 1)
  ("cond does not evaluate false consequent"
   (cond [false (/ 1 0)]
         [true 2])
   => 2)
  ("does not evaluate alternative"
   (cond [true 1]
         [false (/ 1 0)])
   => 1)
  ("evaluates else clause"
   (cond [false 1]
         [else 2])
   => 2))

(define-eval-test-suite
  lambda-tests
  ("captures its environment"
   (((lambda (a) (lambda () a)) 42))
   => 42)
  ("shadows variables"
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

(define-eval-test-suite
  define-tests
  ("binds variable"
   (begin (define x 3) x) => 3)
  ("binds procedure"
   (begin (define (f x) x)
          (f 42))
   => 42)
  ("nests"
   (begin
     (define (f x)
       (define (g x)
         x)
       (+ x (g 1)))
     (f 2))
   => 3))

(define-eval-test-suite
  if-tests
  ("evaluates consequent"
   (if true 1 (/ 1 0)) => 1)
  ("evaluates alternative"
   (if false (/ 1 0) 2) => 2))

(define-eval-test-suite
  set!-tests
  ("mutates variable"
   (begin (define y 1)
          (set! y 2)
          y)
   => 2))

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
(define-test-suite basic-tests
  begin-tests
  cond-tests
  lambda-tests
  quote-tests
  define-tests
  if-tests
  set!-tests
  primitive-diff-tests)

(run-tests basic-tests)

(core:install-special-form `(and ,4-4:eval-and))
(core:install-special-form `(or ,4-4:eval-or))

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

(core:install-special-form `(cond ,(lambda (exp env)
                                     (core:eval (4-5:cond->if exp) env))))

(define-eval-test-suite
  cond-recipient-tests
  ("applies true predicate value to recipient function"
   (cond (1 => (lambda (x) (+ x x))))
   => 2)
  ("works with mix of clauses"
   (cond (false => (lambda (x) 1))
         (false 2)
         (3 => (lambda (x) x))
         (else 42))
   => 3))

(run-tests cond-recipient-tests)

(core:install-special-form `(let ,(lambda (exp env)
                                    (core:eval (4-6:let->combination exp)
                                               env))))

(define-eval-test-suite
  let-tests
  ("basic test"
   (let ((a 1) (b 2)) a) => 1))

(run-tests let-tests)

(core:install-special-form `(let* ,(lambda (exp env)
                                     (core:eval (4-7:let*->nested-lets exp)
                                                env))))

(define-eval-test-suite
  let*-tests
  ("basic test"
   (let* ((a 1)
          (b (+ a 1)))
     (+ a b))
   => 3))

(run-tests let*-tests)
