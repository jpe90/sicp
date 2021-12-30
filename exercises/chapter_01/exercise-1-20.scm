(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

;; with normal-order evaluation, functions calls are fully expanded and then
;; reduced. (remainder a b) would be run more times here than in applicative order.
