#lang sicp
(#%require "streams.rkt")

(define (show x)
  (display-line x)
  x)

(define (test)
  (define x
    (stream-map
     show
     (stream-enumerate-interval 0 10)))

  (display-line "before x 5")
  (stream-ref x 5)
  (display-line "before x 7")
  (stream-ref x 7))
