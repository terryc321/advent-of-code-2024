
(define-macro (swap! x y) ; wrong
  `(let ((tmp ,x))
      (set! ,x ,y)
      (set! ,y tmp)))

(let ((a 1)(b 2))
  (swap! a b)
  (list a b))

