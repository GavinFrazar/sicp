#lang sicp

(#%require "tests.rkt"
           "ddcore.rkt")

(set-eval! eval)
(set-env! the-global-environment)
(run-tests all-eval-tests)
