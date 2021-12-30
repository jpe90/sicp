(define-module (sicp exercises exercise-2-40)
  #:use-module (sicp utils))

(define (enum-interval low high)
  (if (> low high) '()
        (cons low (enum-interval (inc low) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list i j)) (enum-interval 1 (- n 1))))
   (enum-interval 1 n)))

(define (make-pair-sum pair)
  (let ((fst (car pair))
        (snd (cadr pair)))
    (list fst snd (+ fst snd))))

;; naive, this should only need to check up to sqrt(n) but
;; too lazy to implement
(define (prime? n)
  (define (prime-iter i n)
    (cond ((= i n) #t)
          ((= 0 (remainder n i)) #f)
          (else (prime-iter (inc i) n))))
  (if (> 2 n) #f
      (prime-iter 2 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
