(define-module (sicp examples gcd)
  #:export (gcd))

(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

;; If Euclid’s Algorithm requires k steps to compute the GCD of some pair,
;; then the smaller number in the pair must be greater than or equal to the
;; kth Fibonacci number
