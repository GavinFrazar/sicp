#lang racket

(define (make-account balance password)
  (define failed-passwords 0)
  (define (call-the-cops)
    (error "Cops called for too many password fails"))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request:
                MAKE-ACCOUNT" m)]))
  (define (check-pass p m)
    (if (eq? p password)
        (begin (set! failed-passwords 0)
               (dispatch m))
        (begin (set! failed-passwords (+ 1 failed-passwords))
               (if (> failed-passwords 7)
                   (call-the-cops)
                   (λ (amount) "Incorrect password")))))
  check-pass)

(define (make-joint acc pass new-pass)
  (define (dispatch p m)
    (if (eq? p new-pass)
        (acc pass m)
        (λ (amount) "Incorrect password")))
  dispatch)
