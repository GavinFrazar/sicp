#lang sicp

;; import all exercises for the first complete evaluator implementation
(#%require "tests.rkt"
           "ddcore.rkt"
           "4-4.rkt")

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
