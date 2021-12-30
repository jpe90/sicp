(define (even? n)
  (= (remainder n 2) 0))

(define (double a) (+ a a))

(define (halve a) (/ a 2))

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (fast-mult a b)
  (define (fast-mult-iter a b c)
    (cond
      ((= c 0) a)
      ((even? c) (fast-mult-iter a (double b) (halve c)))
      (else (fast-mult-iter (+ a b) b (- c 1)))))
  (fast-mult-iter 0 a b))

