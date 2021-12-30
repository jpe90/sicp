(define-module (sicp examples stream)
  #:export (cons-stream
            integers-starting-from
            integers
            car-stream
            cdr-stream
            stream-null?
            filter-stream
            ref-stream))

;; https://stackoverflow.com/a/24529585
(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result
                       (proc))
                 (set! already-run?
                       #t)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((_ exp)
     (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (force x)
  (x))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (car-stream stream) (car stream))
(define (cdr-stream stream) (force (cdr stream)))

(define (stream-null? stream) (null? stream))

(define (filter-stream pred stream)
  (cond ((stream-null? stream) #nil)
        ((pred (car-stream stream))
         (cons-stream (car-stream stream) (filter-stream pred (cdr-stream stream))))
        (else (filter-stream pred (cdr-stream stream)))))

(define (ref-stream s n)
  (if (= n 0)
      (car-stream s)
      (ref-stream (cdr-stream s) (- n 1))))

(define (map-stream proc s)
  (if (stream-null? s) #nil
      (cons-stream (proc (car-stream s))
                   (map-stream proc (cdr-stream s)))))
(define (for-each-stream proc s)
  (if (stream-null? s)
      'done
      (begin (proc (car-stream s))
             (for-each-stream proc (cdr-stream s)))))
