(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (cube a)
  (* a a a))

(define (square a)
  (* a a))

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (/
   (+ (/ x (square guess)) (* 2 guess))
   3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 27) ;; 2.08

