(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (small-enough-fraction? guess diff)
  (< (abs (/ diff guess)) 0.0001))

(define (sqrt-iter-2 guess x lastguess)
  (if (small-enough-fraction? guess (abs (- guess lastguess)))
      guess
      (sqrt-iter-2 (improve guess x) x guess)))


(define (square a)
  (* a a))

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-2 x)
  (sqrt-iter-2 (improve 1.0 x) x 1.0))

;; with a small enough input, the magnitude of the "good-enough" tolerance will
;; render answers useless

;; Expected: 0.032
(sqrt 0.001)
;; Actual: 0.04124542607499115

;; the new sqrt does not have this issue:

(sqrt-2 0.001)
;; Actual: 0.03162277660168433

;; looking at an example for large inputs:

;; Expected:  5.33752072494e14
;; Actual: fails to terminate
(sqrt 284891274891742891129021892189)

;; pausing execution while program is not terminating in dr racket, and stepping
;; the frozen state:

;; guess => 533752072494096.1
;; x => 284891274891742891129021892189

;; guess => 533752072494096.1
;; x => 284891274891742891129021892189

;; guess => 533752072494096.1
;; x => 284891274891742891129021892189

;; plugging these into improve:
;;
;; (improve 533752072494096.1 284891274891742891129021892189)
;; 533752072494096.1
;;
;; here we see improve is returning the same value as the guess. floating point
;; arithmetic is dangerous because of its imprecision. here we see that a
;; rounding error is making it impossible for 'improve to progress the calculation
;; with the new sqrt:

;; Actual: 533752072494098.5
(sqrt-2 284891274891742891129021892189)

