#lang sicp
(#%require "circuits.rkt")
(#%require "adders.rkt")

(define (ripple-carry-adder A B S C)
  (define (n-adder A B S c-out)
    (if (null? S)
        'ok
        (let ((c-in (make-wire)))
          (full-adder (car A) (car B) c-in (car S) c-out)
          (n-adder (cdr A) (cdr B) (cdr S) c-in))))
  (n-adder A B S C))

;; helpers for testing
(define (get-bits S)
  (map (lambda (wire)
         (get-signal wire))
       S))

(define (base10 bits)
  (define (iter-add bits power sum)
    (if (null? bits)
        sum
        (iter-add (cdr bits)
                  (* power 2)
                  (+ sum (* power (car bits))))))
  (iter-add (reverse bits) 1 0))

(define (wires->num wires)
  (base10 (get-bits wires)))

;; Test
(define (test)
  (define a1 (make-wire))
  (define a2 (make-wire))
  (define a3 (make-wire))

  (define b1 (make-wire))
  (define b2 (make-wire))
  (define b3 (make-wire))

  (define s1 (make-wire))
  (define s2 (make-wire))
  (define s3 (make-wire))

  (let ((A (list a1 a2 a3))
        (B (list b1 b2 b3))
        (S (list s1 s2 s3))
        (C (make-wire)))

    (probe 's1 s1)
    (probe 's2 s2)
    (probe 's3 s3)
    (probe 'Carry C)
    (ripple-carry-adder A B S C)
    (set-signal! a3 1)
    (set-signal! a2 1)
    (set-signal! a1 1)
    (set-signal! b1 1)
    (set-signal! b3 1)
    (propagate)

    (display "A: ")
    (display (wires->num A))
    (newline)
    (display "B: ")
    (display (wires->num B))
    (newline)
    (display "Sum: ")
    (display (wires->num (cons C S)))
    (newline)))
