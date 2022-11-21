#lang sicp

(define (writeln x)
  (display x)
  (newline))
#|
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
|#
;; Using the constraint system
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
	   (set-value! sum
		       (+ (get-value a1) (get-value a2))
		       me))
	  ((and (has-value? a1) (has-value? sum))
	   (set-value! a2
		       (- (get-value sum) (get-value a1))
		       me))
	  ((and (has-value? a2) (has-value? sum))
	   (set-value! a1
		       (- (get-value sum) (get-value a2))
		       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request - ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
	       (and (has-value? m2) (= (get-value m2) 0)))
	   (set-value! product 0 me))
	  ((and (has-value? m1) (has-value? m2))
	   (set-value! product
		       (* (get-value m1) (get-value m2))
		       me))
	  ((and (has-value? m1) (has-value? product))
	   (set-value! m2
		       (/ (get-value product) (get-value m1))
		       me))
	  ((and (has-value? m2) (has-value? product))
	   (set-value! m1
		       (/ (get-value product) (get-value m2))
		       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request - MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request - CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Unknown request - PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
	     (set! value newval)
	     (set! informant setter)
	     (for-each-except setter
			      inform-about-value
			      constraints))
	    ((not (= value newval))
	     (error "Contradiction" (list value newval)))
	    (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
	  (begin (set! informant false)
		 (for-each-except retractor
				  inform-about-no-value
				  constraints))
	  'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
	  (set! constraints
		(cons new-constraint constraints)))
      (if (has-value? me)
	  (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
	     (if informant true false))
	    ((eq? request 'value) value)
	    ((eq? request 'set-value!) set-my-value)
	    ((eq? request 'forget) forget-my-value)
	    ((eq? request 'connect) connect)
	    (else (error "Unknown operation - CONNECTOR"
			 request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? (car items) exception) (loop (cdr items)))
	  (else (procedure (car items))
		(loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)



(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)

;; error
;; (set-value! F 212 'user)

(forget-value! C 'user)

(set-value! F 212 'user)

(writeln "Exercise 3.33")
(define (averager a b c)
  (let ((two (make-connector))
        (temp (make-connector)))
    (multiplier two c temp)
    (adder a b temp)
    (constant 2 two)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(averager a b c)
(probe "A: " a)
(probe "B: " b)
(probe "C: " c)

(set-value! a 18 'user)
(set-value! b 10 'user)

(forget-value! a 'user)
(set-value! c 55 'user)

(forget-value! b 'user)
(forget-value! c 'user)
(set-value! c 3.1415926 'user)
(set-value! b 1 'user)