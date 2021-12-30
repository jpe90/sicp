(define-module (sicp examples huffman)
  #:use-module (sicp utils)
  #:export (sample-message
            sample-tree
            decode))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? x) (eq? (car x) 'leaf))
(define (leaf-symbol x) (cadr x))
(define (leaf-weight x) (caddr x))

;; a tree has a left, a right, a list of remaining symbols, and a weight
(define (make-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
  
(define (symbols tree)
  (if (leaf? tree)
    (list (leaf-symbol tree))
    (caddr tree)))

(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
(define (weight tree)
  (if (leaf? tree)
    (leaf-weight tree)
    (cadddr tree)))

(define (decode tree bits)
  (define (decode-1 current-branch bits)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch current-branch (car bits))))
        (if (leaf? next-branch)
          (cons (leaf-symbol next-branch) (decode-1 tree (cdr bits)))
          (decode-1 next-branch (cdr bits))))))
  (decode-1 tree bits))

(define (choose-branch tree bit)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "1 or 0 only"))))

(define sample-tree (make-tree (make-leaf 'A 4)
                               (make-tree
                                (make-leaf 'B 2)
                                (make-tree
                                 (make-leaf 'D 1)
                                 (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define s1 (make-tree
            (make-leaf 'D 1)
            (make-leaf 'C 1)))
