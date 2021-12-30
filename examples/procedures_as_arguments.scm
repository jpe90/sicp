(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ 2 a))) (pi-sum (+ 4 a) b))))

(pi-sum 1 20)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (double a) (* 2 a))
(define (inc a) (+ 1 a))
;; (sum double 1 inc 3)

(define (integral f a b dx)
  (let ((add-dx (lambda (x) (+ x dx))))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)))

(integral double 1 10 0.01)












































;; (define (integral f a b dx)
;;   (let ((add-dx (lambda (x) (+ x dx))))
;;     (* (sum f (+ a (/ dx 2.0)) add-dx b)
;;        dx)))


