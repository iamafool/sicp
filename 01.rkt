;; Exercise 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))

;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
#lang racket
(define (ex1.3 x y z)
  (cond ((or (>= x y z) (>= y x z)) (+ (* x x) (* y y)))
        ((or (>= x z y) (>= z x y)) (+ (* x x) (* z z)))
        (else (+ (* y y) (* z z)))))

(= (ex1.3 5 8 9)
(ex1.3 5 9 8)
(ex1.3 8 5 9)
(ex1.3 8 9 5)
(ex1.3 9 8 5)
(ex1.3 9 5 8))

;; Exercise 1.4
#lang racket
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

( = (a-plus-abs-b 5 10)
    (a-plus-abs-b 5 -10))#lang racket
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

;; Exercise 1.5
#lang racket
(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))
