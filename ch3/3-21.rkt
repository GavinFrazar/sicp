#lang racket
(#%require "queue.rkt")

(define (print-queue queue)
  (display (front-ptr queue)))
