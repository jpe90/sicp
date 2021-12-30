(define (fold-right f initial seq)
  (if (null? seq)
    initial
    (+ (f (car seq))
       (fold-right f initial (cdr seq)))))

(define (fold-left f initial seq)
  (define (iter result rst)
    (if (null? rst)
      result
      (iter (f result (car rst))
            (cdr rst))))
  (iter initial seq))

;; these will produce the same values for any monoidal operator
;; in other words, an associative binary operation with an identity
;; literally the only benefit of learning haskell has been that
;; i knew (an) answer to this question
;;
;; actually i don't think it needs to have an identity
;; so the operator just needs to be associative
;; and learning haskell had no value 
