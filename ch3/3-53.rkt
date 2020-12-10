#lang sicp
(#%require "streams.rkt")

(define s (cons-stream 1 (add-streams s s)))
;; generates powers of 2 by the recursive relation:
;; s(k) = s(k-1) + s(k-1) = 2*s(k-1)
;; with initial condition:
;; s(0) = 1
;;
;; implies: s(1) = 2, s(2) = 4, s(3) = 8, etc
