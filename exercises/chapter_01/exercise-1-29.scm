(define (cube a) (* a a a))

;; (define (simpson f a b n)
;;   (let ((h (/ (- b a) n))
;;         (y (lambda (k) (f (+ a (* k h))))))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define y (lambda (k) (f (+ a (* k h)))))
  
  (sum )
  )

;; the other dude grouped the terms and rewrote it
