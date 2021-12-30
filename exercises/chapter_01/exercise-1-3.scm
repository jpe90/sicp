;; 1.3

(define (biggest a b)
  (if (> a b) a b))

(define (smallest a b)
  (if (> a b) b a))

(define (biggest-of-three a b c)
  (biggest c (biggest a b)))

(define (middle-of-three a b c)
  (smallest c (biggest a b)))

(define (square a)
  (* a a))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (sum-of-greater-two-squares a b c)
  (sum-of-squares (biggest-of-three a b c)
                  (middle-of-three a b c)))

(sum-of-greater-two-squares 1 2 3) ;; 13
