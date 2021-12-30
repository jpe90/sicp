(define-module (sicp examples rationals)
  #:use-module (sicp examples gcd))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; (define make-rat cons)
;; (define numer car)
;; (define denom cdr)
;; (define (make-rat n d) (cons n d))

(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (print-rat x) (newline)
  (display (numer x)) (display "/") (display (denom x)))

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

;; (print-rat one-third)
