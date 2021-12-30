(define-module (sicp exercises exercise-3-33)
  #:use-module (sicp example constraints))

(define (averager a b x)
  (let ((d (make-connector))
        (u (make-connector)))
    (adder a b u)
    (multiplier u d x)
    (constant (/ 1 2) d)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define x (make-connector))

(probe "answer" x)

(averager a b x)

(set-value! a 4 'user)
(set-value! b 8 'user)









