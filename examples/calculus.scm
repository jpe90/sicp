(define-module (sicp examples calculus)
  #:export (make-sum
            make-product
            deriv))

(define (variable? x) (symbol? x))

(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product a1 a2) (list '* a1 a2))


(define (sum? x) (and (pair? x) (eq? '+ (car x))))
(define (addend s) (cdr s))
(define (augund s) (caddr s))

(define (product? x) (and (pair? x) (eq? '* (car x))))
(define (multiplier p) (cdr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var)
                           1
                           0))
         ((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augund exp) var)))
         ((product? exp) (make-sum
                         (make-product (multiplier exp) var)
                         (make-product (multiplicand exp) var)))))

;; (deriv '(+ x 3) 'x)

