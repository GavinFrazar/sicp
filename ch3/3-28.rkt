#lang racket
(require "circuits.rkt")

(define (logical-or a b)
  (cond [(= (logical-not a) 0) 1]
        [(= (logical-not b) 0) 1]
        [else 0]))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1)
                                 (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (test)
 (define a (make-wire))
 (define b (make-wire))
 (define out (make-wire))
 (probe 'a a)
 (probe 'b b)
 (probe 'out-or out)
 (or-gate a b out)

 (propagate)

 (set-signal! a 1)
 (propagate)

 (set-signal! a 0)
 (propagate)

 (set-signal! b 1)
 (propagate)

 (set-signal! b 0)
 (propagate))

(provide (all-defined-out))
