
;; break problem down to fundamentally small unit of comprehension

;; (inc? xs)
;;        is xs an increasing set of values
;; (dec? xs)
;;        is xs a decreasing set of values
;;
;; (inc? '(1 2 1))  => #f
;; (inc? '(1 2 3 4 5)) => #t
;;
;; (dec? '(5 4 3 2 1 )) => #t
;; (dec? '(5 4 3 4 1 )) => #f

(define inc? (lambda (xs)
	      (cond
	       ((null? xs) #t)
	       (#t (let ((h (car xs))
			 (t (cdr xs)))
		     (inc2 h t))))))

(define inc2 (lambda (h t)
	       (cond
		((null? t) #t)
		(#t (let ((h2 (car t))
			  (t2 (cdr t)))
		      (if (<= h h2)       
			  (inc2 h2 t2)
			  #f))))))


(define dec? (lambda (xs)
	      (cond
	       ((null? xs) #t)
	       (#t (let ((h (car xs))
			 (t (cdr xs)))
		     (dec2 h t))))))

(define dec2 (lambda (h t)
	       (cond
		((null? t) #t)
		(#t (let ((h2 (car t))
			  (t2 (cdr t)))
		      (if (>= h h2)       
			  (dec2 h2 t2)
			  #f))))))






