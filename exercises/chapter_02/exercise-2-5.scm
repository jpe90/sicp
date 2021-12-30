(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
  (define (count-div-2 a cnt)
    (if (= 0 (remainder a 2)) 
        (count-div-2 (/ a 2) (inc cnt))
        cnt))
  (count-div-2 x 0))

(define (cdr x)
  (define (count-div-3 a cnt)
    (if (= 0 (remainder a 3)) 
        (count-div-3 (/ a 3) (inc cnt))
        cnt))
  (count-div-3 x 0))

