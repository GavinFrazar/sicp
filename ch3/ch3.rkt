#lang racket

;; Exercise 3.1
(define (make-accumulator init-val)
  (let ([acc init-val])
    (λ (val)
      (begin (set! acc (+ val acc))
             acc))))

;; Exercise 3.2
(define (make-monitored f)
  (define counter 0)
  (define (get-counter)
    counter)
  (define (reset-counter)
    (set! counter 0))
  (define (dispatch msg)
    (cond [(and (symbol? msg)
                (eq? msg 'how-many-calls?))
           (get-counter)]
          [(and (symbol? msg)
                (eq? msg 'reset-counter))
           (reset-counter)]
          [else (begin (set! counter (+ counter 1))
                       (f msg))]))
  dispatch)

;; Exercise 3.3
(define (make-account balance password)
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
        (dispatch m)
        (λ (amount) "Incorrect password")))
  check-pass)

;; Exercise 3.4
(define (make-account-2 balance password)
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
