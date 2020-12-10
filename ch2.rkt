#lang racket
(require "ch1.rkt")
(require sicp-pict)

(define (average a b)
  (/ (+ a b) 2))
(define (square x)
  (* x x))
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (neg-rat x)
  (make-rat (* (numer x))
            (denom x)))
(define (sub-rat x y)
  (add-rat x (neg-rat y)))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (reciprocal x)
  (make-rat (denom x) (numer x)))
(define (div-rat x y)
  (mul-rat x (reciprocal y)))
(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))
(define (make-rat num denom)
  (let ((g (gcd num denom)))
    (cond ((> (* num denom) 0)
           (cons (abs (/ num g)) (abs (/ denom g))))
          (else
           (cons (* -1 (abs (/ num g))) (abs (/ denom g)))))))

(define (numer x)
  (car x))
(define (denom x)
  (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Exercise 2.2
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s) (y-point (end-segment s))))))
(define (length-segment s)
  (sqrt (+ (square (- (x-point (start-segment s))
                      (x-point (end-segment s))))
           (square (- (y-point (start-segment s))
                      (y-point (end-segment s)))))))
(define (rect-perimeter r)
  (+ (* 2 (length-segment (rect-L r))) (* 2 (length-segment (rect-W r)))))
(define (rect-area r)
  (* (length-segment (rect-L r)) (length-segment (rect-W r))))
(define (make-rect p1 p2 p3)
  (cons p1 (cons p2 p3)))
(define (rect-L r)
  (make-segment (car r) (car (cdr r))))
(define (rect-W r)
  (make-segment (car r) (cdr (cdr r))))


;; (define (cons x y)
;;   (define (dispatch m)
;;   (cond ((= m 0) x)
;;         ((= m 1) y)
;;         (else (error "Argument not 0 or 1 -- CONS" m))))
;;   dispatch)
;; (define (car pair)
;;   (pair 0))
;; (define (cdr pair)
;;   (pair 1))

;; Exercise 2.4
;; (define (cons x y)
;;   (lambda (m) (m x y)))
;; (define (car z)
;;   (z (lambda (p q) p)))
;; (define (cdr z)
;;   (z (lambda (p q) q)))

;; Exercise 2.5
;; (define (cons a b)
;;   (* (expt 2 a) (expt 3 b)))
;; (define (car z)
;;   (define (iter z acc)
;;     (cond ((odd? z) acc)
;;           (else (iter (/ z 2) (inc acc)))))
;;   (iter z 0))
;; (define (cdr z)
;;   (define (iter z acc)
;;     (cond ((> (remainder z 3) 0) acc)
;;           (else (iter (/ z 3) (inc acc)))))
;;   (iter z 0))

;; Exercise 2.6
;; Church numerals
;; (define zero (lambda (f) (lambda (x) x)))
;; (define one (lambda (f) (lambda (x) (f x))))
;; (define two (lambda (f) (lambda (x) (f (f x)))))
;; (define (add-1 n)
;;   (lambda (f) (lambda (x) (f ((n f) x)))))
;; (define (+ a b)
;;   (lambda (f) (lambda (x) ((a f) ((b f) x)))))
;; (define (* a b)
;;   (lambda (f) (lambda (x) ((a (b f)) x))))
;; (define three (+ one two))
;; (define (int-to-church n)
;;   (define (iter n f)
;;     (cond ((= n 0) f)
;;           (else (iter (dec n) (add-1 f)))))
;;   (iter n zero))
;; (define (church-to-int n)
;;   ((n inc) 0))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
(define (make-interval a b)
  (cons a b))

;; Exercise 2.7
(define (lower-bound i)
  (car i))
(define (upper-bound i)
  (cdr i))

;; Exercise 2.8
(define (sub-interval x y)
  (add-interval x
                (make-interval (* -1 (upper-bound y))
                               (* -1 (lower-bound y)))))

(define nil '())
(define (list-ref list ref)
  (define (iter list n)
    (cond ((= n 0) (car list))
          (else (iter (cdr list) (dec n)))))
  (iter list ref))

;; (define (length list)
;;   (define (iter list count)
;;     (cond ((null? list) count)
;;           (else (iter (cdr list) (inc count)))))
;;   (iter list 0))
;; (define (append list1 list2)
;;   (define (iter remaining-items acc)
;;     (cond ((null? remaining-items) acc)
;;           (else (iter (cdr remaining-items) (cons (car remaining-items) acc)))))
;;   (iter (reverse list1) list2))

;; Exercise 2.17
(define (last-pair list)
  (cond ((null? list) nil)
        ((null? (cdr list)) (car list))
        (else (last-pair (cdr list)))))

;; Exercise 2.18
(define (reverse list)
  (define (iter remaining-items acc)
    (cond ((null? remaining-items) acc)
          ((pair? remaining-items)
           (iter (cdr remaining-items)
                 (cons (car remaining-items) acc)))
          (else remaining-items)))
  (iter list nil))

;; Exercise 2.19 -- TODO

;; Exercise 2.20
(define (same-parity . items)
  (define (iter remaining-items acc parity)
    (cond ((null? remaining-items) acc)
          ((= (remainder (car remaining-items) 2) parity)
           (iter (cdr remaining-items)
                 (cons (car remaining-items) acc)
                 parity))
          (else (iter (cdr remaining-items) acc parity))))
  (cond ((null? items) nil)
        (else (reverse (iter items nil (remainder (car items) 2))))))

;; (define (map f items)
;;   (define (iter remaining-items acc)
;;     (cond ((null? remaining-items) acc)
;;           (else (iter (cdr remaining-items)
;;                       (cons (f (car remaining-items))
;;                             acc)))))
;;   (reverse (iter items nil)))

;; Exercise 2.23
(define (for-each action items)
  (define (iter remaining-items)
    (cond ((null? remaining-items) #t)
          (else (action (car remaining-items))
                (iter (cdr remaining-items)))))
  (iter items))

;; (define (count-leaves tree)
;;   (cond ((null? tree) 0)
;;         ((pair? tree) (+ (count-leaves (car tree))
;;                          (count-leaves (cdr tree))))
;;         (else 1)))
;; (define (count-leaves tree)
;;   (define (iter tree to-count acc)
;;     (cond ((null? tree) (if (null? to-count) acc
;;                             (iter (car to-count) (cdr to-count) acc)))
;;           ((pair? tree) (iter (car tree) (cons (cdr tree) to-count) acc))
;;           (else (if (null? to-count)
;;                     (inc acc)
;;                     (iter (car to-count) (cdr to-count) (inc acc))))))
;;   (iter tree nil 0))

;; Exercise 2.27
(define (deep-reverse items)
  (cond ((null? items) items)
        ((pair? items)
         (reverse (cons (deep-reverse (car items))
                        (deep-reverse (cdr items)))))
        (else items)))
(define x (list (list 1 2) (list 3 4)))

;; Exercise 2.28
(define (fringe tree)
  (cond ((null? tree) tree)
        ((pair? tree) (append (fringe (car tree))
                              (fringe (cdr tree))))
        (else (list tree))))

;; Exercise 2.29
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

(define (weigh structure)
  (cond
   ((null? structure) 0)
   ((pair? structure)
    (+ (weigh (branch-structure (left-branch structure)))
       (weigh (branch-structure (right-branch structure)))))
   (else structure)))

(define (balanced? mobile)
  (cond
   ((pair? mobile)
    (and (= (* (branch-length (left-branch mobile))
               (weigh (branch-structure (left-branch mobile))))
            (* (branch-length (right-branch mobile))
               (weigh (branch-structure (right-branch mobile)))))
         (balanced? (branch-structure (left-branch mobile)))
         (balanced? (branch-structure (right-branch mobile)))))
   (else #t)))

;; tests
;; (define m (make-mobile (make-branch 1 10) (make-branch 1 5)))
;; (define n (make-mobile (make-branch 3 m) (make-branch 3 m)))
;; (define p (make-mobile (make-branch 10 7) (make-branch 5 14)))
;; (define q (make-mobile (make-branch 3 p) (make-branch 3 p)))
;; (define r (make-mobile (make-branch 3 p) (make-branch 2 p)))

;; (balanced? m)
;; (balanced? n)
;; (balanced? p)
;; (balanced? q)
;; (balanced? r)

;; Exercise 2.30
;; Direct solution
;; (define (square-tree tree)
;;   (cond ((null? tree) tree)
;;         ((pair? tree) (cons (square-tree (car tree))
;;                             (square-tree (cdr tree))))
;;         (else (square tree))))
;; Solution using map
;; (define (square-tree tree)
;;   (map
;;    (lambda (sub-tree)
;;      (cond ((pair? sub-tree) (square-tree sub-tree))
;;            (else (square sub-tree))))
;;    tree))

;; Exercise 2.31
;; (define (map-tree f tree)
;;   (cond ((null? tree) tree)
;;         ((pair? tree)
;;          (cons (map-tree f (car tree))
;;                (map-tree f (cdr tree))))
;;         (else (f tree))))
(define (map-tree f tree)
  (map
   (lambda (sub-tree) (cond ((pair? sub-tree)
                             (map-tree f sub-tree))
                            (else (f sub-tree))))
   tree))
(define (square-tree tree) (map-tree square tree))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (head) (cons (car s) head)) rest)))))

(define (accumulate op initial sequence)
  (cond [(null? sequence) initial]
        [(pair? sequence) (op (car sequence)
                              (accumulate op initial (cdr sequence)))]))

;; Exercise 2.33
;; (define (map p sequence)
;;   (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append seq1 seq2)
  (if (or (pair? seq2) (null? seq2))
      (accumulate cons seq2 seq1)
      (accumulate cons (list seq2) seq1)))

(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

;; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff acc-sum)
                (+ this-coeff (* acc-sum x)))
              0
              coefficient-sequence))

;; Exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (fringe t))))

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (λ (row) (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ (row) (matrix-*-vector cols row)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (cond [(null? sequence) initial]
        [else (op (car sequence) (fold-right op initial (cdr sequence)))]))

(define (reverse-v2 sequence)
  (define (push value sequence)
    (fold-right cons (list value) sequence))
  (fold-right (λ (elem acc) (push elem acc)) '() sequence))

(define (reverse-v3 sequence)
  (fold-left (λ (acc elem) (cons elem acc)) nil sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (flatten seq)
  (flatmap (λ (x) x) seq))

;; Exercise 2.40
(define (unique-pairs n)
  (flatmap (λ (i) {map (λ (j) {list j i}) (range 1 i)})
       (range 2 (+ n 1))))

(define (unique-triples n)
  (flatmap (λ (k) {map (λ (pair) {append pair (list k)})
                       (unique-pairs (- k 1))})
       (range 3 (+ n 1))))

;; Exericse 2.41 -- not quite 3Sum is it
(define (sum-pair pair)
  (+ (car pair) (cadr pair)))

(define (two-sum goal-sum)
  {filter (λ (pair) {= (sum-pair pair) goal-sum})
          (unique-pairs goal-sum)})

(define (sum-triple triple)
  (+ (car triple) (sum-pair (cdr triple))))

(define (three-sum n goal-sum)
  (filter (λ (triple) {= goal-sum (sum-triple triple)})
          (unique-triples n)))

;; Generalized for my own amusement:
(define (unique-k-tuples k n)
  (cond [(= k 2) (unique-pairs n)]
        [else (flatmap (λ (x) {map (λ (j-tuple) {append j-tuple (list x)})
                                   (unique-k-tuples (- k 1) (- x 1))})
                       (range k (+ n 1)))]))

(define (sum-k-tuple k k-tuple)
  (cond [(= k 2) (sum-pair k-tuple)]
        [else (+ (car k-tuple)
                 (sum-k-tuple (- k 1) (cdr k-tuple)))]))

(define (k-sum k n goal-sum)
  (filter (λ (k-tuple) {= goal-sum (sum-k-tuple k k-tuple)})
          (unique-k-tuples k n)))

;; Exercise 2.42 -- TODO fix this solution idk what is wrong with it
(define (make-queen row col)
  (cons row col))

(define (get-row queen)
  (car queen))

(define (get-col queen)
  (cdr queen))

(define (diff-queen a b)
  (list (abs (- (car a) (car b)))
        (abs (- (cadr a) (cadr b)))))

(define (adjoin-position new-row k rest-of-queens)
  (cond [(null? rest-of-queens) (list (make-queen new-row k))]
        [else (cons (make-queen new-row k) rest-of-queens)]))

(define empty-board '())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (!= a b)
  (not (= a b)))

(define (safe? k positions)
  (define (safe-one? a b)
    (cond [(null? b) #t]
          [else ((let ((diff (diff-queen a b)))
                   (and (!= (get-row a) (get-row b))
                        (and (> (get-row diff) 0)
                             (> (get-col diff) 0)))))]))
  (filter (λ (queens)
            (displayln positions)
            {fold-left (λ (acc queen)
                         {and acc (safe-one? (car queens) queen)})
                       #t (cdr queens)})
          positions))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
          (flatmap
          (lambda (rest-of-queens)
            (displayln rest-of-queens)
            (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; Derivatives

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list '+ a1 a2)]))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (make-product m1 m2 . rest-terms)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list '* m1 m2)]))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (make-exponentiation b e)
  (cond [(=number? b 0) 0]
        [(=number? e 0) 1]
        [(=number? e 1) b]
        [else (list '** b e)]))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
          (if (same-variable? exp var) 1 0)]
        [(sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var))]
        [(product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp)))]
        [(exponentiation? exp)
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                              (make-sum (exponent exp) '-1)))]
        [else
          (error "unknown expression type -- DERIV" exp)]))

(define (parse-exp exp)
  (define (parse-right exp result)
    (cond [(null? exp) result]
          [(eq? (car exp) '+) (make-sum result (parse-exp (cdr exp)))]
          [(eq? (car exp) '*)
           (parse-right (cddr exp)
                        (make-product result (parse-exp (cadr exp))))]
          [else (displayln "ERROR -- parse-right")]))

  (cond [(number? exp) exp]
        [(variable? exp) exp]
        [(pair? exp)
         (if (null? (cdr exp))
             (parse-exp (car exp))
             (parse-right (cdr exp) (parse-exp (car exp))))]
        [else (displayln "ERROR -- parse-exp")]))
;; (x + 3 * y + 4)
;;

;; Sets

;; For my sanity
(define false #f)
(define true #t)

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((= x (car set)) true)
;;         ((< x (car set)) false)
;;         (else (element-of-set? x (cdr set)))))

;; Exercise 2.61
;; (define (adjoin-set x set)
;;   (define (insert-in-order x set)
;;     (cond [(null? set) (list x)]
;;           [(< x (car set)) (cons x set)]
;;           [else (cons (car set)
;;                       (insert-in-order x (cdr set)))]))

;;   (if (element-of-set? x set)
;;       set
;;       (insert-in-order x set)))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
              (cons x1
                    (intersection-set (cdr set1)
                                      (cdr set2))))
              ((< x1 x2)
              (intersection-set (cdr set1) set2))
              ((< x2 x1)
              (intersection-set set1 (cdr set2)))))))

;; Exercise 2.59
;; (define (union-set set1 set2)
;;   (cond [(null? set1) set2]
;;         [(null? set2) set1]
;;         [(element-of-set? (car set1) set2)
;;          (union-set (cdr set1) set2)]
;;         [else (cons (car set1)
;;                     (union-set (cdr set1) set2))]))

;; Exercise 2.62
(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond [(= x1 x2)
                       (cons x1
                             (union-set (cdr set1)
                                        (cdr set2)))]
                      [(< x1 x2)
                       (cons x1
                             (union-set (cdr set1)
                                        set2))]
                      [(< x2 x1)
                       (cons x2
                             (union-set set1 (cdr set2)))]))]))

(define (entry tree) (car tree))

(define (left-child tree) (cadr tree))

(define (right-child tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
        (element-of-set? x (left-child set)))
        ((> x (entry set))
        (element-of-set? x (right-child set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
        (make-tree (entry set)
                    (adjoin-set x (left-child set))
                    (right-child set)))
        ((> x (entry set))
        (make-tree (entry set)
                    (left-child set)
                    (adjoin-set x (right-child set))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-child tree))
              (cons (entry tree)
                    (tree->list-1 (right-child tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-child tree)
                      (cons (entry tree)
                            (copy-to-list (right-child tree)
                                          result-list)))))
  (copy-to-list tree '()))
