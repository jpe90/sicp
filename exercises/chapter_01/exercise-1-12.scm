(define (pascal n)
 (define (pascal-row-col row col)
   (if (or (= col 0) (= row col))
       1
       (+ (pascal-row-col (- row 1) col)
          (pascal-row-col (- row 1) (- col 1)))))
  (define (pascal-iter row col cnt)
    (cond ((> col row) (pascal-iter (+ row 1)
                                    0
                                    cnt))
          ( (= cnt 0) (pascal-row-col row col))
          (else (pascal-iter row
                             (+ col 1)
                             (- cnt 1)))))
 (pascal-iter 0 0 n))
