
(import (chicken syntax))
;; the import defines er-macro-transformer


;; here is the hygienic for loop
(define-syntax forloop
  (syntax-rules ()
    ((forloop start stop exps ...)
     (let ((j stop))
       (do ((i start (+ i 1)))
           ((> i j))
         exps ...)))))

;; whilst definintion above will do exprs n times there is no
;; access to the variable i in the for loop
;; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;;
;; analogy in c
;; the ? is a variable no code can access
;; 
;; (for ?=0 ; ? < 10 ; ? ++){
;;      ... exprs ...
;;      }
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;; rename > + j do let 
(define-syntax forloop2
  (er-macro-transformer
   (lambda (exp rename compare)
     (let ((i (car (cdr exp)))
	   (start (car (cdr (cdr exp))))
	   (stop (car (cdr (cdr (cdr exp)))))
	   (exprs (cdr (cdr (cdr (cdr exp)))))
	   (_j (rename 'j))
	   (_> (rename '>))
	   (_+ (rename '+))
	   (_let (rename 'let))
	   (_do (rename 'do)))
       `(,_let ((,_j ,stop))
	  (,_do ((,i ,start (,_+ ,i 1)))
	      ((,_> ,i ,_j))
	    ,@exprs))))))


;; in the for loop we actually want i to be captured
;;want i to be unhygienic.
;; > (forloop2 i 1 10 (write "i=") (write i) (newline))

;; "i="1
;; "i="2
;; "i="3
;; "i="4
;; "i="5
;; "i="6
;; "i="7
;; "i="8
;; "i="9
;; "i="10

;; (define-syntax forloop3
;;   (ir-macro-transformer
;;    (lambda (exp rename compare)
;;      (let* ((decl (car (cdr exp)))
;; 	    (var (car decl))
;; 	    (start (car (cdr decl)))
;; 	    (stop  (car (cdr (cdr decl))))
;; 	    (by (if (null? (cdr (cdr (cdr decl))))
;; 		    1
;; 		    (car (cdr (cdr (cdr decl))))))
;; 	    (exprs (cdr (cdr exp)))
;; 	    (_j (rename 'j))
;; 	    (_> (rename '>))
;; 	    (_< (rename '<))
;; 	    (_<= (rename '<=))
;; 	    (_>= (rename '>=))	    
;; 	    (_+ (rename '+))
;; 	    (_- (rename '-))	    
;; 	    (_let (rename 'let))
;; 	    (_do (rename 'do))
;; 	    (_start (rename 'start))
;; 	    (_stop (rename 'stop))
;; 	    (_from (rename 'from))
;; 	    (_to (rename 'to))
;; 	    (_by (rename 'by))
;; 	    (_loop (rename 'loop)))
;;        `(let ((,_from ,start)
;; 	      (,_to ,stop)
;; 	      (,_by ,(abs ,by)))
;; 	  (cond
;; 	   ((,_<= ,_from ,_to)
;; 	    (letrec  ((,_loop
;; 		       (lambda (,var)    
;; 			 (write "var=") (write var) (newline)
;; 			 (cond
;; 			  ((,_<= i to) #f)
;; 			  (#t (,_loop (,_- i 1)))))))
;; 	      (,_loop ,_from)))
;; 	   (#t
;; 	    (letrec  ((,_loop
;; 		       (lambda (,var)    
;; 			 (write "var=") (write ,var) (newline)
;; 			 (cond
;; 			  ((,_>= i to) #f)
;; 			  (#t (,_loop (+ i 1)))))))
;; 	      (,_loop ,_from)))))))))


;; rename > + j do let
;; (for (i 1 10 1) ...)  optional by statement
;; ;; (for (i 10 1 -2) ...)
;; (define-syntax forloop3
;;   (er-macro-transformer
;;    (lambda (exp rename compare)
;;      (let* ((decl (car (cdr exp)))
;; 	    (var (car decl))
;; 	    (start (car (cdr decl)))
;; 	    (stop  (car (cdr (cdr decl))))
;; 	    (by (if (null? (cdr (cdr (cdr decl))))
;; 		    1
;; 		    (car (cdr (cdr (cdr decl))))))
;; 	    (exprs (cdr (cdr exp)))
;; 	    (_j (rename 'j))
;; 	    (_> (rename '>))
;; 	    (_< (rename '<))
;; 	    (_<= (rename '<=))
;; 	    (_>= (rename '>=))	    
;; 	    (_+ (rename '+))
;; 	    (_- (rename '-))	    
;; 	    (_let (rename 'let))
;; 	    (_do (rename 'do))
;; 	    (_start (rename 'start))
;; 	    (_stop (rename 'stop))
;; 	    (_from (rename 'from))
;; 	    (_to (rename 'to))
;; 	    (_by (rename 'by))
;; 	    (_loop (rename 'loop)))
;;        `(let ((,_from ,start)
;; 	      (,_to ,stop)
;; 	      (,_by ,(abs ,by)))
;; 	  (cond
;; 	   ((,_<= ,_from ,_to)
;; 	    (letrec  ((,_loop
;; 		       (lambda (,var)    
;; 			 (write "var=") (write var) (newline)
;; 			 (cond
;; 			  ((,_<= i to) #f)
;; 			  (#t (,_loop (,_- i 1)))))))
;; 	      (,_loop ,_from)))
;; 	   (#t
;; 	    (letrec  ((,_loop
;; 		       (lambda (,var)    
;; 			 (write "var=") (write ,var) (newline)
;; 			 (cond
;; 			  ((,_>= i to) #f)
;; 			  (#t (,_loop (+ i 1)))))))
;; 	      (,_loop ,_from)))))))))




#|
(let ((from 10)(to 1)(by 1))
  (letrec  ((loop (lambda (i)    
		    (write "i=") (write i) (newline)
		    (cond
		     ((<= i to) #f)
		     (#t (loop (- i 1)))))))
    (loop from)))


(let ((from 1)(to 10)(by 1))
  (letrec  ((loop (lambda (i)    
		    (write "i=") (write i) (newline)
		    (cond
		     ((>= i to) #f)
		     (#t (loop (+ i 1)))))))
    (loop from)))

|#




      


;; in the for loop we actually want i to be captured
;;want i to be unhygienic.
;;(forloop3 (i 1 10) (write "i=") (write i) (newline))
;;(forloop3 (i 1 10 2) (write "i=") (write i) (newline))

;; 1 - debugging - support for macros ?
;; 2 - what is in scope 
(define-syntax swappy!
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((x (cadr form)) (y (caddr form)))
        `(let ((tmp ,x))
            (set! ,x ,y)
            (set! ,y tmp))))))

(import-for-syntax (only srfi-1 second))

;; (define-syntax frig
;;   (er-macro-transformer
;;    (lambda (expr rename compare?)
;;      (let ((a (second expr)))
;;        `(,a)))))


(import-for-syntax (only bindings bind bindable?))

(define-syntax up-for
  (er-macro-transformer
   (lambda (exp rename compare?)
     (let ((%letrec (rename 'letrec))
	   (%loop (rename 'loop))
	   (%lambda (rename 'lambda))
	   (%cond (rename 'cond))
	   (%stop (rename 'stop))
	   (%<= (rename '<=))
	   (%>= (rename '>=))
	   (%> (rename '>))	   
	   (%+ (rename '+))
	   (%start (rename 'start)))
       (bind (for (var start stop) . body) exp
	     `(,%letrec
	       ((,%loop (,%lambda (,var)
				  (when (,%<= ,var ,stop) 
				    ,@body
				    (,%loop (,%+ ,var 1))))))			    
	       (,%loop ,start)))))))



(define-syntax up-for-by
  (er-macro-transformer
   (lambda (exp rename compare?)
     (let ((%letrec (rename 'letrec))
	   (%loop (rename 'loop))
	   (%lambda (rename 'lambda))
	   (%cond (rename 'cond))
	   (%stop (rename 'stop))
	   (%>= (rename '>=))	   
	   (%<= (rename '<=))
	   (%+ (rename '+))
	   (%start (rename 'start)))
       (bind (for (var start stop by) . body) exp
	     `(,%letrec
	       ((,%loop (,%lambda (,var)
				  ;;(write "i=") (write ,(inject 'var)) (newline)
				  (when (,%<= ,var ,stop) 
				    ,@body
				    (,%loop (,%+ ,var ,(abs by)))))))
	       (,%loop ,start)))))))


(define-syntax down-for
  (er-macro-transformer
   (lambda (exp rename compare?)
     (let ((%letrec (rename 'letrec))
	   (%loop (rename 'loop))
	   (%lambda (rename 'lambda))
	   (%cond (rename 'cond))
	   (%stop (rename 'stop))
	   (%<= (rename '<=))
	   (%>= (rename '>=))	   
	   (%- (rename '-))
	   (%start (rename 'start)))
       (bind (for (var start stop) . body) exp
	     `(,%letrec
	       ((,%loop (,%lambda (,var)
				  (when (,%>= ,var ,stop) 
				    ;;(write "i=") (write ,(inject 'var)) (newline)
				    ,@body
				    (,%loop (,%- ,var 1))))))
	       (,%loop ,start)))))))


;;(for (i 1 5) (write "i=")(write i) (newline))
;;(downfor (i 5 1) (write "i=")(write i) (newline))


(define-syntax down-for-by
  (er-macro-transformer
   (lambda (exp rename compare?)
     (let ((%letrec (rename 'letrec))
	   (%loop (rename 'loop))
	   (%lambda (rename 'lambda))
	   (%cond (rename 'cond))
	   (%stop (rename 'stop))
	   (%<= (rename '<=))
	   (%>= (rename '>=))	   
	   (%- (rename '-))
	   (%start (rename 'start)))
       (bind (for (var start stop by) . body) exp
	     `(,%letrec
	       ((,%loop (,%lambda (,var)
				  (when (,%>= ,var ,stop) 
				    ;;(write "i=") (write ,(inject 'var)) (newline)
				    ,@body
				    (,%loop (,%- ,var ,(abs by)))))))
	       (,%loop ,start)))))))



;; (define-syntax big-for
;;   (er-macro-transformer
;;    (lambda (exp rename compare?)
;;      (let ((%letrec (rename 'letrec))
;; 	   (%loop (rename 'loop))
;; 	   (%lambda (rename 'lambda))
;; 	   (%cond (rename 'cond))
;; 	   (%stop (rename 'stop))
;; 	   (%>= (rename '>=))	   
;; 	   (%+ (rename '+))
;; 	   (%start (rename 'start)))
;;        (cond
;; 	((bindable? (for (var start stop) . body) exp)
;; 	 (bind (for (var start stop) . body) exp
;; 	       `(cond
;; 		 ((<= ,start ,stop) (for ,@(cdr exp)))
;; 		 (#t (downfor ,@(cdr exp))))))
;; 	((bindable? (for (var start stop by) . body) exp)
;; 	 `(for-by ,@(cdr exp))))))))



;;(((lambda (x) (lambda (y) (+ x y))) x) y)
;; \x y -> x + y

;; at compile time does it matter if re-bind same expression a few times again ?
(define-syntax for
  (er-macro-transformer
   (lambda (exp rename compare?)
     (let ((%letrec (rename 'letrec))
	   (%loop (rename 'loop))
	   (%lambda (rename 'lambda))
	   (%cond (rename 'cond))
	   (%stop (rename 'stop))
	   (%>= (rename '>=))	   
	   (%+ (rename '+))
	   (%start (rename 'start)))
       (cond
	((bindable? (for (var start stop) . body) exp)
	 (bind (for (var start stop) . body) exp
	       `(if (<= ,start ,stop)
		    (up-for ,@(cdr exp))
		    (down-for ,@(cdr exp)))))
	((bindable? (for (var start stop by) . body) exp)
	 (bind (for (var start stop by) . body) exp
	       `(if (<= ,start ,stop)
		    (up-for-by ,@(cdr exp))
		    (down-for-by ,@(cdr exp))))))))))
	 



;;(ppe '(for (i 1 9 (write "i=")(write i) (newline))))
;;(for (i 1 10) (write "i=")(write i) (newline))
;;(for (i 1 10 2) (write "i=")(write i) (newline))
;;(for (i 10 1) (write "i=")(write i) (newline))
;;(for (i 9 1 -2) (write "i=")(write i) (newline))
;;(for (i 9 1 2) (write "i=")(write i) (newline))

;; (define-syntax for
;;   (er-macro-transformer
;;    (lambda (exp rename compare?)
;;      (let ((pat '(for (var start stop) . body)))
;;        (cond
;; 	((bindable? pat exp)
;; 	 (let ((%letrec (rename 'letrec))
;; 	       (%loop (rename 'loop))
;; 	       (%lambda (rename 'lambda))
;; 	       (%cond (rename 'cond))
;; 	       (%stop (rename 'stop))
;; 	       (%+ (rename '+))
;; 	       (%start (rename 'start)))
;; 	   (bind pat exp
;; 		 `(,%letrec
;; 		   ((,%loop (,%lambda (,var)
;; 				      ;;(write "i=") (write ,(inject 'var)) (newline)
;; 				      ,@body
;; 				      (,%cond
;; 				       ((,%>= ,var ,stop) #f)
;; 				       (#t (,%loop (,%+ ,var 1)))))))
;; 		   (,%loop ,start))))))))))











       



