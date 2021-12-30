;; 1.6

;; The function will infinitely bounce between calls to new-if and sqrt-iter.
;; If is a special form which conditionally evaluates its second and third
;; arguments. Because scheme has applicative ordering, function calls always
;; evaluate arguments their arguments. One of the arguments to sqrt-iter
;; is a call to new-if, and one of the arguments to new-if is sqrt-iter.
;; Nothing will stop these functions from calling each other.



