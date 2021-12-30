(define (A x y) (cond ((= y 0) 0)
                      ((= x 0) (* 2 y))
                      ((= y 1) 2)
                      (else (A (- x 1) (A x (- y 1))))))
;; What are the values of the following expressions?
(A 1 10)

;; bubbles down (A 0 (A 0 (A 0 ... (A 0 1)))
;; then bubbles back up (A 0 (A 0 ... (A 0 2)
;; which multiplies 2 by 2 for each expansion
;; aka 2^10 = 1024

(A 2 4)
;; computes 2^16 = 65536

(A 3 3)
;; computes 2^16 = 65536
