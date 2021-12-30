(define-module (sicp examples lambda-calculus))

(define (mycons x y) (lambda (m) (m x y)))
(define (mycar x) (x (lambda (a d) a)))
(define (mycdr x) (x (lambda (a d) d)))





