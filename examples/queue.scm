(define-module (sicp examples queue)
  #:export (make-queue
            front-queue
            insert-queue!
            delete-queue!
            empty-queue?))

(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "you fucking idiot the queue is empty")
      (car (front-ptr queue))))

(define (empty-queue? queue)
  (eq? (front-ptr queue) '()))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond
     ((empty-queue? queue)
      (set-front-ptr! queue new-pair)
      (set-rear-ptr! queue new-pair)
      queue)
     (else
      (set-cdr! (rear-ptr queue) new-pair)
      (set-rear-ptr! queue new-pair)
      queue))))


(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

(define (print-queue queue)
  (define (print-elem queue)
    (if (null? queue)
        '()
        (begin
          (display (car queue))
          (print-elem (cdr queue)))))
  (display "queue: ")
  (print-elem (car queue)))

(print-queue q1)
;; (define (op-and-print))




