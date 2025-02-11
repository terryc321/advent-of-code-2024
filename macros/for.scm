

#|

consideration writing macro

1 . evaluation of arguments once and only once , then use result many times
due to side effects

2 . 


** useage **

(for (i from to by) ...)

from 10 downto 1 by -2 , right think that -2 steps ,
do an absolute value of by then subtract or increase from to as appropriate


|#

(define-macro (for e . body)
  (let ((sym (car e))
	(from (car (cdr e)))
	(to (car (cdr (cdr e))))
	(by (if (null? (cdr (cdr (cdr e))))
		1
		(car (cdr (cdr (cdr e))))))
	(loop (gensym "loop"))
	(sto (gensym "to"))
	(sby (gensym "by"))	
	(sfrom (gensym "from")))
    `(let ((,sto ,to) ;; single evaluation on 
	   (,sfrom ,from)
	   (,sby (abs ,by))) ;; 
       (cond
	((< ,sto ,sfrom)
	 (letrec ((,loop (lambda (,sym)
			   (cond
			    ((< ,sym ,sto) #f)
			    (#t ,@body
				(,loop (- ,sym ,sby)))))))
	   (,loop ,sfrom)))
	(#t (letrec ((,loop (lambda (,sym)
			   (cond
			    ((> ,sym ,sto) #f)
			    (#t ,@body
				(,loop (+ ,sym ,sby)))))))
	      (,loop ,sfrom)))))))

#|
;; (for i from 1 to 10 by -1 loops infinite ... probabyl not what wanted)
;; so have an absolute on the step 

(ppe '(for (i 10 1)
	      (fmt #t "i = ~a~%" i)))

(for (i 3 1)
  (fmt #t "i = ~a~%" i))

(for (i 5 1 -2)
  (fmt #t "i = ~a~%" i))

(for (i 1 5 -2)
  (fmt #t "i = ~a~%" i))


(for (i 1 3)
	(fmt #t "i = ~a~%" i))

(do-for (x 1 3)
	(do-for (y 1 3)
		(fmt #t "x = ~a : y = ~a~%" x y)))


(ppe '(do-for (x 1 3)
	      (do-for (y 1 3)
		      (fmt #t "x = ~a : y = ~a~%" x y))))
     
  
(do-for (x 1 12 2)
	(fmt #t "x step in 2 = ~a ~%" x ))



(do-for (i 0 10)
	(fmt #t "i = ~a~%" i))

(do-for (i 1 3)
	(fmt #t "i = ~a~%" i))

(do-for (x 1 3)
	(do-for (y 1 3)
		(fmt #t "x = ~a : y = ~a~%" x y)))


(ppe '(do-for (x 1 3)
	      (do-for (y 1 3)
		      (fmt #t "x = ~a : y = ~a~%" x y))))
     
  
(do-for (x 1 12 2)
	(fmt #t "x step in 2 = ~a ~%" x ))

|#


