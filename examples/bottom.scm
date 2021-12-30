(define e1 (lambda (x n)
   (cond ((= n 0) 1)
         (else
          (* x (foo x (n - 1)))))))

(define e2 (lambda (x n)
   (cond ((= n 0) 1)
         (else
          (* x (e2 x (n - 1)))))))

(define e3 (lambda (x n)
   (cond ((= n 0) 1)
         (else
          (* x (e3 x (n - 1)))))))

(define F
  (lambda (g)
    (lambda (x n)
      (cond ((= n 0) 1)
            (else 
             (* x (g x (- n 1))))))))



(define Y
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))

(define expt
  (lambda (x n)
    (cond ((= n 0) 1)
          (else 
           (* x (expt x (- n 1)))))))

(define (f a n)
  (if (= n 0)
      (cos a)
      (cos (f a (- n 1)))))

