;; 1.5

(define (p) (p))
(define (test x y)
   (if (= x 0) 0 y))

(test 0 (p))

;; With applicative order, the interpreter will evaluate arguments first
;; and then apply them to procedures. In this case, it will try to evaluate
;; test's arguemnts "0" and "(p)". 0 evaluates to 0 because it is a primitive
;; value, while (p) is defined as (p), and will infinitely recur when evaluated.
;; Scheme has applicative ordering, so this is the behavior observed by running
;; this code.
;;
;; In normal order, functions are evaluated before their arguments. In this case,
;; "test" will be evaluated first. It is defined with the special form "if", which
;; is evaluated first. It evaluates its first argument, (= x 0), where x is bound to 0,
;; which evaluates to true. According to the behavior of the "if" form, it evaluates
;; the true case, which evaluates to 0. It never evaluates the parameter y, bound to
;; (p), due to the the fact that "if" form has special behavior which does not
;; unconditionally evaluate all arguments like a normal form would.
