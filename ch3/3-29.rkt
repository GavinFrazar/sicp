#lang sicp
(#%require "circuits.rkt")

(define (or-gate a1 a2 output)
  (let ((a1-inv-out (make-wire))
        (a2-inv-out (make-wire))
        (and-out (make-wire)))
    (inverter a1 a1-inv-out)
    (inverter a2 a2-inv-out)
    (and-gate
     a1-inv-out
     a2-inv-out
     and-out)
    (inverter and-out output)))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(define out (make-wire))

(or-gate input-1 input-2 out)
(probe 'out out)

(propagate)
