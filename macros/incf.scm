

;; vulnerable to reassignment of 
;; set!
;; +

(define-macro (incf! x)
  `(set! ,x (+ ,x 1)))

(let ((a 1))
  (incf! a)
  a)


