#lang racket
(provide (all-defined-out))
(define (square x) (* x x))
(define (sum-of-squares a b) (+ (square a) (square b)))
(define (>= a b) (not (< a b)))
(define (pick-biggest a b) (if (>= a b) a b))
(define (pick-smallest a b) (if (>= a b) b a))
(define (sum-of-squares-of-two-biggest a b c)
        (sum-of-squares
            (pick-biggest a b)
            (pick-biggest (pick-smallest a b) c)))
(define (sqrt-iter guess x)
  (cond ((good-enough? guess x) guess)
        (else (sqrt-iter (improve guess x) x))))
(define (average a b)
  (/ (+ a b) 2))
(define (improve guess x)
  (average (/ x guess) guess))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.00001))
;; (define (sqrt x)
;;   (sqrt-iter 1.0 x))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; chapter 1.2

(define (factorial n)
  (if (= n 1)
      n
      (* (factorial (- n 1)) n)))

(define (tail-rec-factorial acc n)
  (if (= n 1)
      acc
      (tail-rec-factorial (* n acc) (- n 1))))
(define (fact n)
  (tail-rec-factorial 1 n))


(define (dec n)
  (- n 1))
(define (inc n)
  (+ n 1))

(define (count-change amount)
  (define (denom-of-first-coin kinds-of-coins)
    (cond
     ((= kinds-of-coins 1) 1)
     ((= kinds-of-coins 2) 5)
     ((= kinds-of-coins 3) 10)
     ((= kinds-of-coins 4) 25)
     ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond
     ((= amount 0) 1)
     ((or (< amount 0) (= kinds-of-coins 0)) 0)
     (else (+ (cc amount (dec kinds-of-coins))
              (cc (- amount (denom-of-first-coin kinds-of-coins)) kinds-of-coins)))))
  (cc amount 5))

;; Exercise 1.11
(define (rec-f n)
  (cond
   ((< n 3) n)
   (else (+ (rec-f (- n 1)) (* 2 (rec-f (- n 2))) (* 3 (rec-f (- n 3)))))))
(define (iter-f n)
  (define (inc k) (+ k 1))
  (define (helper n counter acc1 acc2 acc3)
    (cond
     ((< n 3) n)
     ((= counter n) (+ (* 3 acc1) (* 2 acc2) acc3))
     (else (helper
            n
            (inc counter)
            acc2
            acc3
            (+ (* 3 acc1) (* 2 acc2) acc3)))))
  (helper n 3 0 1 2))

;; Exercise 1.12
(define (pascal-elem row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal-elem (- row 1) (- col 1))
         (pascal-elem (- row 1) col))))

;; Exercise 1.16
;; (define (fast-exp number power)
;;   (define (fast-exp-iter number acc counter steps)
;;     (println steps)
;;     (cond
;;      ((or (= power 0) (= power counter)) acc)
;;      ((= (* counter 2) power) (square acc))
;;      ((<= (* 4 counter) power)
;;       (fast-exp-iter number (square acc)(* 2 counter) (inc steps)))
;;      (else
;;       (fast-exp-iter number (* acc number) (inc counter) (inc steps)))))
;;   (fast-exp-iter number number 1 1))

;; ideal version
(define (fast-exp base power)
  (define (iter acc base power)
    (cond ((= power 0) acc)
          ((even? power) (iter acc (square base) (/ power 2)))
          (else (iter (* acc base) base (dec power)))))
  (iter 1 base power))

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (cube x) (fast-exp x 3))
(define (identity x) x)
(define (integral f a b dx)
  (* dx (sum f (+ a (/ dx 2.0)) b (lambda (x) (+ x dx)))))

;; (define (sum f a b next)
;;   (cond ((> a b) 0)
;;         (else (+ (f a) (sum f (next a) b next)))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (add-two x) (+ x 2))
  (* (/ h 3)
     (+ (f a)
        (f (+ a (* n h)))
        (* 2 (sum y 2 (- n 2) add-two))
        (* 4 (sum y 1 (- n 1) add-two)))))
(define (sum f a b next)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (+ a acc))))
  (iter a 0))

;; (define (product f a b next)
;;   (cond ((> a b) 1)
;;         (else (* (f a) (product f (next a) b next)))))

(define (product f a b next)
  (define (iter a acc)
    (cond ((> a b) acc)
          (else (iter (next a) (* (f a) acc)))))
  (iter a 1))

(define (accumulate f a b next combiner identity-elem)
  (define (iter a acc)
    (cond ((> a b) acc)
          (else (iter (next a) (combiner (f a) acc)))))
  (iter a identity-elem))

(define (fact-3 n)
  (product identity 1 n inc))

(define (approx-pi k)
  (define (f n) (- n (remainder n 2)))
  (define (g n) (- n (remainder (inc n) 2)))
  (* 4.0 (/ (product f 3 (+ k 2) inc) (product g 3 (+ k 2) inc))))

(define (filtered-accumulate f a b next combiner identity-elem constraint)
  (define (iter a acc)
    (cond ((> a b) acc)
          ((constraint a) (iter (next a) (combiner (f a) acc)))
          (else (iter (next a) acc))))
  (iter a identity-elem))

(define (fixed-point f initial-guess)
  (define tolerance 0.00001)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (cond ((good-enough? next guess) next)
            (else (try next)))))
  (try initial-guess))

(define (sqrt-fixed-point x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; Exercise 1.35
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; Exercise 1.36
(define find-x
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; (define (sqrt x)
;;   (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define dx 0.00001)
(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x))
                 dx)))
(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x) ((derive f) x)))))

(define (newton-method f guess)
  (fixed-point (newton-transform f) guess))

;; (define (sqrt x)
;;   (newton-method (lambda (y) (- (square y) x))
;;                  1.0))

(define (fixed-point-of-transform f transform guess)
  (fixed-point (transform f) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

;; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

;; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))
