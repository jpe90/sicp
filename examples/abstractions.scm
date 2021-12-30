;; cons is a function that returns a function which takes an input and returns the first
;; captured value if its 0 or the second if its 1

(define (cons x y)
  (define (dispatch m)
    (cond
      ((= m 0) x)
      ((= m 1) y)))
  dispatch)

;; car and cdr are functions which take a function and call it, providing a 0 to
;; the function returned by cons in the event of car and 1 in the event of cdr

(define (car z) (z 0))
(define (cdr z) (z 1))

(cdr (cons 1 3))

(define (map lst fn)
  (if (null? lst)
    '()
    (cons (fn (car lst)) (map (cdr lst) fn))))

(mymap '(1 2 3) (lambda (x) (* 2 x)))
