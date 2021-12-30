;; we need some kind of way of squaring

;; (Tpq (Tpq (a,b))
;; Tpq =
;; a <- bq + aq + ap
;; b <- bp + aq
;; (Tpq (Tpq (a,b)) =
;; ( (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;; , (bp + aq)p + (bq + aq + ap)q)
;; =
;; ( (bpq + aq^2) + (bq^2 + aq^2 + apq) + (bqp + aqp + ap^2)
;; , (bp^2 + aqp) + (bq^2 + aq^2

(define (fib n) (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   2 ; compute p′
                   2 ; compute q′
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
