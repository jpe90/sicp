(define (last-element lst)
  (if (null? (cdr lst))
      (car lst)
      (last-element (cdr lst))))

(last-element (list 1 2 3 4))

