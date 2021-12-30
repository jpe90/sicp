(define-module (sicp examples sequences)
  #:use-module (sicp utils)
  #:export (fold-left
            fold-right
            filter
            accumulate
            fib
            flatmap
            make-pair
            prime-sum-pairs
            ))

(define (fold-right f initial seq)
  (if (null? seq)
      initial
      (f (car seq) 
         (fold-right f initial (cdr seq)))))

;; (1 2 3)
;; (op 1 (2 3))
;; .. (op 1 2 3 ())
;;    (op 1 (op 2 (op 3 0)))

(define (fold-left f initial seq)
  (define (iter result rst)
    (if (null? rst)
        result
        (iter (f result (car rst))
              (cdr rst))))
  (iter initial seq))

;; (op 1 2)
;; (op 3 3)
;; 6

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond
    ;; terminate on the nulls returned from cdr or cadr at a leaf
    ((null? tree) '())
    ;; if its a leaf, return the leaf wrapped in a lust
    ((not (pair? tree)) (list tree))
    ;; append is called on lists and combines their elements
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))))

(define (square x) (* x x))

(define (sum-odd-squares tree) (accumulate
                                + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (even-fibs n) (accumulate
                       cons
                       '()
                       (filter even? (map fib (enumerate-interval 0 n)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime? n)
  (define (prime-iter i n)
    (cond ((= i n) #t)
          ((= 0 (remainder n i)) #f)
          (else (prime-iter (inc i) n))))
  (if (> 2 n) #f
      (prime-iter 2 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(prime-sum-pairs 6)
