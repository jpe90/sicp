(define (p a b)
  (if (= a 0)
      b
      (inc (p (dec a) b))))

(p 4 5)
;; (if (= 4 0)
;;   5
;;   (inc (p (dec 4) 5))) 
;; ... (inc 3 5)
;; ... (inc 2 5)
;; ... (inc 1 5)
;; (if (= 0 0)
;;   5 
;;   ..) --> 5
;; ->> (inc (inc (inc (inc 5))))

;; this is a recursive function with a base case of a = 0

(define (p2 a b)
  (if (= a 0)
      b
      (p2 (dec a) (inc b))))

;; (if (= 4 0)
;;   b
;;   (p2 3 6))
;; ... (p2 2 7)
;; ... (p2 1 8)
;; ... (p2 0 9)
;; ... 9
;; 
;; this is a linear iterative process whose state can be summarized by variables a and b



