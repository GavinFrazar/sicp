#lang sicp
(#%require "queue.rkt")

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond [(eq? m 'get-signal)
             signal-value]
            [(eq? m 'set-signal!)
             set-my-signal!]
            [(eq? m 'add-action!)
             accept-action-procedure!]
            [else (error "Unknown operation
                          WIRE" m)]))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire proc)
  ((wire 'add-action!) proc))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))


(define (logical-not s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else (error "Invalid signal" s)]))

(define (logical-and a b)
  (cond [(= (logical-not a) (logical-not b) 0) 1]
        [else 0]))

;; define gates
(define (inverter input output)
  (define (invert-input)
    (let ((new-value
           (logical-not (get-signal input))))
      (after-delay
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (probe name wire)
  (add-action!
   wire
   (lambda ()
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display " New-value = ")
     (display (get-signal wire))
     (newline))))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (make-time-segment clock-time queue)
  (cons clock-time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda clock-time)
  (set-car! agenda clock-time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! clock-time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< clock-time
           (segment-time (car segments)))))
  (define (make-new-time-segment clock-time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment clock-time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) clock-time)
        (insert-queue!
         (segment-queue (car segments))
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment
                      clock-time
                      action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment
                clock-time
                action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments!
         agenda
         (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty:
              FIRST-AGENDA-ITEM")
      (let ((first-seg
             (first-segment agenda)))
        (set-current-time!
         agenda
         (segment-time first-seg))
        (front-queue
         (segment-queue first-seg)))))

(define the-agenda (make-agenda))

(#%provide (all-defined))
