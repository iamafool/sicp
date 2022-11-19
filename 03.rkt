#lang sicp

(define (writeln x)
  (display x)
  (newline))

(writeln "Exercise 3.1")
(define (make-accumulator x)
  (let ((sum x))
    (lambda (y)
      (set! sum (+ sum y))
      sum)))

(define a (make-accumulator 5))
(a 10)
(a 10)

(writeln "Exercise 3.2")
(define (make-monitored x)
  (let ((count 0))
    (lambda (y)
      (cond ((eq? y 'how-many-calls?) count)
          ((eq? y 'reset-count) (set! count 0))
          (else
             (set! count (+ count 1))
             (x y))))))

(define s (make-monitored sqrt))
(s 100)
(s 25)
(s 144)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)

(writeln "Exercise 3.3")
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password1 m)
    (cond ((eq? password password1)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request - MAKE-ACCOUNT"
                              m))))
          (else (error "Incorrect password"))))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 50)
;; ((acc 'some-other-password 'withdraw) 60)


(writeln "Exercise 3.12")

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)
(define w (append! x y))
w
(cdr w)

(writeln "Exercise 3.13")
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z1 (make-cycle (list 'a 'b 'c)))
z1
;; (last-pair z1)


(writeln "Exercise 3.14")
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w1 (mystery v))
w1