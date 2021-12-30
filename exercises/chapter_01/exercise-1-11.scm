;; f n = 3 if n < 3
;; f n = f (n-1) + 2 f(n-2) + 3 f (n-3)

;; so:
;; fn 2 -> 3
;; fn 3 -> 3 + (2 * 3) + (3 * 3) = 18
;; fn 4 -> 18 + (2 * 3) + (3 * 3) = 33
;; fn 5 -> (3 * 3) + ((2 * 3) 
;; fn 4 -> 3 + 2(f

(define (fib n)
  (define (fib-iter a b n)
    (if (= 0 n)
        b
        (fib-iter (+ a b) a (- n 1))))
  (fib-iter 1 0 n))

(fib 3)

(define (fn-rec n)
  (if (> 3 n)
      n
      (+ (fn-rec (- n 1)) (* 2 (fn-rec (- n 2))) (* 3 (fn-rec (- n 3))))))

(define (fn n)
  (define (fn-iter a b c n)
    (if (> 3 n)
        a
        (fn-iter (+ a (* 2 b) (* 3 c))
                 a
                 b
                 (- n 1))))
  (fn-iter 2 1 0 n))

(fn-rec 4) ;; 11
(fn 4) ;; 11
