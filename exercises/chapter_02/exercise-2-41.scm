(define-module (sicp exercises exercise-2-41)
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
            (list i j)) (enum-interval 1 (- i 1))))
   (enum-interval 1 n)))

(define (unique-triplets n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
        (map (lambda (k) 
               (list i j k))
             (enum-interval 1 (- j 1))))
      (enum-interval 1 (- i 1))))
   (enum-interval 1 n)))



