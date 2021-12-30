(define (cube x) (* x x x))
(define (p x) (- (* 3 x)
                 (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)

;; (sine 12.15)
;; (sine 4.05)
;; (sine 1.3499999999999999)
;; (sine 0.4499999999999996)
;; (sine 0.15)
;; (sine 0.0499999999999996)

;; p is applied 5 times

;; Because the algorithm divides its input by a constant factor every iteration
;; until it hits a threshold value, it has logarithmic time and space complexity
