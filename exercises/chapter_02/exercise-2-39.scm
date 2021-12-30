(define (fold-right f initial seq)
  (if (null? seq)
    initial
    (f (car seq) 
       (fold-right f initial (cdr seq)))))

(define (fold-left f initial seq)
  (define (iter result rst)
    (if (null? rst)
      result
      (iter (f result (car rst))
            (cdr rst))))
  (iter initial seq))

(define (right-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (left-reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(left-reverse (list 1 2 3 4 5))
