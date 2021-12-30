(define-module (sicp utils)
  #:export (square
            append
            inc))

(define (inc x) (+ 1 x))

(define (square x) (* x x))

(define (append this other)
  (if (null? this)
    other
    (cons (car this) (append (cdr this) other))))

