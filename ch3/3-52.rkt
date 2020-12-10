#lang sicp
(#%require "streams.rkt")

(define sum 0)
(define (show-sum msg)
  (newline)
  (display msg)
  (display " ")
  (display sum)
  (newline))
(show-sum "init") ; sum=0

(define (accum x)
  (set! sum (+ x sum))
  sum)
(show-sum "accum") ; sum=0

(define seq
  (stream-map
   accum
   (stream-enumerate-interval 1 20)))
(show-sum "seq") ; sum=1

(define y (stream-filter even? seq))
(show-sum "y") ; sum=6

(define z
  (stream-filter
   (lambda (x)
     (= (remainder x 5) 0)) seq))
(show-sum "z") ; sum=10

(stream-ref y 7) ; sum=7th even number of the accumulation sequence = 120
(show-sum "ref y 7")

(display-stream z) ; sum=210
(show-sum "display z")

;; Since stream evaluation is delayed, sum will only take on the accumulated
;; value of `seq` as needed for evaluation of each statement.
;; Had we defined `delay` as a function instead of a special form, then
;; sum would take on the value of 210 immediately after defining `seq`,
;; since 210 is the last value of the sequence.
