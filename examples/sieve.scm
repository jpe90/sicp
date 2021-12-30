(define-module (sicp examples sieve)
  #:use-module (sicp examples stream))


;; maybe enum interval on 1 to x?
(define (sieve stream)
  (cons-stream
   (car-stream stream)
   (sieve (filter-stream (lambda (x)
                           (not (= (remainder x (car-stream stream)) 0)))
                         (cdr-stream stream)))))
(define primes (sieve (integers-starting-from 2)))

