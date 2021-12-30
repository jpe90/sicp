(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square a)
  (* a a))

;; iterative version of below

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; an iterative version will track state variables and a running total
;; exponent n
;; base b
;; state variable a
;; state transformation in way ab^n unchanged
;; a 1 at start and has answer at end
;; b^n ends as 1 

;; i screwed this up initially because i thought i should always keep the
;; b parameter constant across calls, but it needs to double when n is even

(define (fast-expt2 b n)
  (define (fast-expt-iter a b n)
    (cond
      ((= n 0) a)
      ((even? n) (fast-expt-iter a (square b) (/ n 2)))
      (else (fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter 1 b n))

(fast-expt 2 2)
(fast-expt2 2 2)


