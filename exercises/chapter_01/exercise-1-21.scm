(define (find-divisor a b)
  (if (= 0 (remainder a b))
      b
      (find-divisor a (inc b))))


(define (smallest-divisor n) (find-divisor n 2))
