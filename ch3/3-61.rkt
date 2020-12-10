#lang sicp
(#%require "streams.rkt")
(#%require "3-59.rkt")
(#%require "3-60.rkt")

(define (invert-unit-series s)
  (define X (cons-stream
             1 (mul-series (scale-stream (stream-cdr s) -1)
                           X)))
  X)

;; (define (invert-series s)
;;   (scale-stream
;;    (invert-unit-series s)
;;    (/ 1 (stream-car s))))

;; (define (invert-series s)
;;   (invert-unit-series
;;    (scale-stream s (/ 1 (stream-car s)))))

(define (invert-series s)
  (define X
    (let ((C (stream-car s)))
      (if (= C 0)
          (error "C was 0
                  INVERT-SERIES")
          (scale-stream
           (cons-stream
            1
            (mul-series
             (scale-stream (stream-cdr s)
                           -1)
             X))
           (/ 1 C)))))
  X)


;; S = C + S_r
;; XS = 1
;; X(C+S_r) = 1
;; XC + XS_r = 1
;; XC = 1 - XS_r
;; X = 1/C - XS_r/C
(define (test)
  (let* ((x (cons-stream 42 integers))
           (x-inv (invert-series x)))
    (display-line (stream-take x 10))
    (display-line (stream-take x-inv 10))
    (newline)
    (stream-take (mul-series x x-inv) 10)))

(#%provide (all-defined))
