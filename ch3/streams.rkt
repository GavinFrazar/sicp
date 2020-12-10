#lang sicp

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((_ expression) (memo-proc (lambda () expression)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (force promise)
  (promise))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-null? s)
  (null? s))

(define the-empty-stream '())

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc
                         (stream-cdr s)))))

(define (stream-filter predicate s)
  (if (stream-null? s)
      the-empty-stream
      (if (predicate (stream-car s))
          (cons-stream (stream-car s)
                       (stream-filter predicate
                                      (stream-cdr s)))
          (stream-filter predicate
                         (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

;; utilities
(define (display-line s)
  (newline)
  (display s))

(define (display-stream s)
  (stream-for-each display-line s))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (sieve s)
  (cons-stream
   (stream-car s)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible?
                   x (stream-car s))))
           (stream-cdr s)))))

(define (integers-starting-from n)
  (cons-stream
   n
   (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define primes
  (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map
   (lambda (x) (* x factor))
   s))

(define (stream-take s n)
  (if (= n 0)
      '()
      (cons (stream-car s)
            (stream-take (stream-cdr s)
                         (- n 1)))))


(define (sqrt-improve guess x)
  (define (average a b)
    (/ (+ a b)
       2.))
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess)
                   (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define (partial-sums s)
    (cons-stream (stream-car s)
                 (add-streams (partial-sums s)
                              (stream-cdr s))))

(define pi-stream
  (scale-stream
   (partial-sums (pi-summands 1)) 4))

(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(#%provide (all-defined))

