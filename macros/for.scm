

#|

consideration writing macro

1 . evaluation of arguments once and only once , then use result many times
due to side effects

2 . 


** useage **

(for (i from to by) ...)

from 10 downto 1 by -2 , right think that -2 steps ,
do an absolute value of by then subtract or increase from to as appropriate



;; (import-for-syntax bindings procedural-macros scheme (chicken format))
;; (import (chicken format))

;; bind example joins x <- 1 , y <- 2 
;;(bind (x y) '(1 2) (list x y))

(bind (for (sym from to) . body)
        '(for (i 0 10) a b c d e)
         (format #t "sym -> [~a]~%" sym)  
         (format #t "from -> [~a]~%" from)  
         (format #t "to-> [~a]~%" to)  
(format #t "body -> [~a]~%" body))


     

  here with implicit renaming , we avoid a lot of unneccesary renaming
  if we happen to miss one , its renamed automatically
  in order to capture variable exit we must inject exirt into expression
   ,(inject 'exit)
  do we need gensyms on implicit-renaming transformer ??
  expression differes from define-macro becuase expr represents entire expression
no just expression args given to for macro like in efine macro

rather than try to manually destructure using car cdrs we perhaps can use bind

optional arguments (by 1) 

case 1 : includes BY 

    (bind (for (sym from to by) . body) expr ...< macro goes here > )

case 2 : no BY clause assumed step is 1 (implicit assumption) 

    (bind (for (sym from to) . body) expr ...< macro goes here > )

|# 


;; (import procedural-macros)
;; (import-for-syntax
;;  (chicken base)
 
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))


;; ;; implicit renaming 
;; (define-syntax for
;;   (ir-macro-transformer
;;    (lambda (expr inject compare)
;;      (bind (for (sym from to) . body) expr
;; 	   (let ((sfrom (gensym "from"))
;; 		 (sto (gensym "to"))
;; 		 (sby (gensym "by"))
;; 		 (loop (gensym "loop")))
;; 	     `(let ((,sfrom ,from)
;; 		    (,sto ,to) 		
;; 		    (,sby abs 1))) ;; 
;; 		(letrec ((,loop (lambda (,,(inject sym))
;; 				  (cond
;; 				   ((> ,,(inject sym) ,sto) #f)
;; 				   (#t ,@body
;; 				       (,loop (+ ,,(inject sym) ,sby)))))))
;; 		  (,loop ,sfrom)))))))



;; https://github.com/mnieper/scheme-macros

(module for-macros (for)

  (import scheme)
  (import (chicken base))
  (import (chicken syntax))
  (import procedural-macros)
  (import srfi-1)

  ;; (import-for-syntax scheme)
  ;; (import-for-syntax
  ;;  (only scheme length)
  ;;  (only srfi-1 second)
  ;;  (only bindings bind bindable?)
  ;;  procedural-macros)

  ;; (begin-for-syntax
  ;;  (import srfi-1))

  (import-for-syntax srfi-1)
  (import-for-syntax bindings)
  (import-for-syntax scheme)

  ;;(import procedural-macros)
  
  
  ;; (import-for-syntax
  ;;    (only procedural-macros define-for-syntax er-macro-transformer ir-macro-transformer)
  ;;    (only bindings bind bindable?))
  
  

#|
(define-syntax swap! ; wrong
  (er-macro-transformer
    (lambda (form rename compare?)
      (let ((x (cadr form)) (y (caddr form)))
        `(let ((tmp ,x))
           (set! ,x ,y)
           (set! ,y tmp))))))
|#


#|
;; & bind from incoming expression in a very limited brittle way
;; %%%%%%%%%%%%% below works !!
(define-syntax up-for
  (er-macro-transformer
   (lambda (expr rename compare?)
     (bind (for (sym from to) . body)
	   expr
	   (let ((%> (rename '>))
		 (%+ (rename '+))
		 (%from (rename 'from))
		 (%to (rename 'to))
		 (%by (rename 'by))
		 (%loop (rename 'loop)))
	     `(let ((,%from ,from)
		    (,%to ,to)
		    (,%by 1))
		(letrec ((,%loop (lambda (,sym)
				   (cond
				    ((,%> ,sym ,%to) #f)
				    (#t ,@body
					(,%loop (,%+ ,sym ,%by)))))))
		  (,%loop ,%from))))))))



(define-syntax down-for
  (er-macro-transformer
   (lambda (expr rename compare?)
     (bind (for (sym from to) . body)
	   expr
	   (let ((%< (rename '<))
		 (%- (rename '-))
		 (%from (rename 'from))
		 (%to (rename 'to))
		 (%by (rename 'by))
		 (%loop (rename 'loop)))
	     `(let ((,%from ,from)
		    (,%to ,to)
		    (,%by 1))
		(letrec ((,%loop (lambda (,sym)
				   (cond
				    ((,%< ,sym ,%to) #f)
				    (#t ,@body
					(,%loop (,%- ,sym ,%by)))))))
		  (,%loop ,%from))))))))


;;(ppe '(for (i 1 10)  (format #t "i = ~a~%" i)))
;;(ppe '(down-for (i 10 1)  (format #t "i = ~a~%" i)))


(define-syntax up-for-by
  (er-macro-transformer
   (lambda (expr rename compare?)
     (bind (for (sym from to by) . body)
	   expr
	   (let ((%> (rename '>))
		 (%+ (rename '+))
		 (%from (rename 'from))
		 (%to (rename 'to))
		 (%by (rename 'by))
		 (%loop (rename 'loop)))
	     `(let ((,%from ,from)
		    (,%to ,to)
		    (,%by ,(abs by)))
		(letrec ((,%loop (lambda (,sym)
				   (cond
				    ((,%> ,sym ,%to) #f)
				    (#t ,@body
					(,%loop (,%+ ,sym ,%by)))))))
		  (,%loop ,%from))))))))


;;(ppe '(for (i 1 100 2)  (format #t "i = ~a~%" i)))

(define-syntax down-for-by
  (er-macro-transformer
   (lambda (expr rename compare?)
     (bind (for (sym from to by) . body)
	   expr
	   (let ((%< (rename '<))
		 (%- (rename '-))
		 (%from (rename 'from))
		 (%to (rename 'to))
		 (%by (rename 'by))
		 (%loop (rename 'loop)))
	     `(let ((,%from ,from)
		    (,%to ,to)
		    (,%by ,(abs by)))
		(letrec ((,%loop (lambda (,sym)
				   (cond
				    ((,%< ,sym ,%to) #f)
				    (#t ,@body
					(,%loop (,%- ,sym ,%by)))))))
		  (,%loop ,%from))))))))

;;(ppe '(down-for-by (i 100 1 -2)  (format #t "i = ~a~%" i)))
|#


  
(define-syntax up-down-for-by
  (er-macro-transformer
   (lambda (expr rename compare?)
     (bind (for (sym from to by) . body)
	   expr
	   (let ((%> (rename '>))
		 (%+ (rename '+))
		 (%from (rename 'from))
		 (%to (rename 'to))
		 (%by (rename 'by))
		 (%loop (rename 'loop)))
	     `(let ((,%from ,from)
		    (,%to ,to)
		    (,%by ,(abs by)))
		(letrec ((,%loop (lambda (,sym)
				   (cond
				    ((,%> ,sym ,%to) #f)
				    (#t ,@body
					(,%loop (,%+ ,sym ,%by)))))))
		  (,%loop ,%from))))))))


 (define foo (lambda (expr rename compare?)
	       expr))


 ;; the wrong macro can keep macro-expander busy indefinitely
 ;; (for (sym from to by) ...)
 ;; (for (sym from to)  


 (define-syntax foo2   ;; %%%%%%%%%%%%%
   (er-macro-transformer
     (lambda (e r c)
       "uses by expression")))
 

  (define-syntax foo3 ;;%%%%%%%%
    (er-macro-transformer
     (lambda (expr rename compare?)
       "has no by expression")))

  ;; second is undefined ?

  (begin-for-syntax 
   (define macro-second (lambda (e) (car (cdr e))))

   (import-for-syntax (chicken format))
  
  (define-syntax for ;; %%%%%%%%%%
    (er-macro-transformer
     (lambda (expr rename compare?)
       (let ((second-expr-len (length (macro-second expr))))
	 (format #t "the for macro has length ~a~%" second-expr-len)
	 (cond
	  ((= second-expr-len 4) (foo2))
	  ((= second-expr-len 3) (foo2))
	  (#t (error "wtf")))))))
  
  )


  
  
 








;;      (foo expr rename compare?))))

;; )


     ;; (let ((pat1 #f)
     ;; 	   (pat2 #f))
     ;; (cond
     ;;  ((bindable? '(for (sym from to by) . body) expr) `(up-for-by . ,@(cdr expr))) 
     ;;  ((bindable? '(for (sym from to) . body) expr) `(up-for . ,@(cdr expr)))
     ;;  (#t (error "cannot")))))))

   


#|
     (bind (for (sym from to by) . body)
	   expr
	   (let ((%< (rename '<))
		 (%- (rename '-))
		 (%from (rename 'from))
		 (%to (rename 'to))
		 (%by (rename 'by))
		 (%loop (rename 'loop)))
	     `(let ((,%from ,from)
		    (,%to ,to)
		    (,%by ,(abs by)))
		(letrec ((,%loop (lambda (,sym)
				   (cond
				    ((,%< ,sym ,%to) #f)
				    (#t ,@body
					(,%loop (,%- ,sym ,%by)))))))
		  (,%loop ,%from))))))))
|#



) ;; *********** end of module declaration ***************************


#|

(begin 
  (for (i 1 10)  (format #t "i = ~a~%" i))
  (for-by (i 1 10 2)  (format #t "i = ~a~%" i))
  (down-for (i 10 1)  (format #t "i = ~a~%" i))
  (down-for-by (i 10 1 2)  (format #t "i = ~a~%" i))
  )

|#











;; test
;; 

	   



#|
writing scheme macros is an utter cluster fuck


(define-syntax for
  (ir-macro-transformer
   (lambda (expr inject compare)
     (letrec ((generic-for-loop (lambda (inject body sym from to by)       
				  (let ((loop (gensym "loop"))
					(sto (gensym "to"))
					(sby (gensym "by"))	
					(sfrom (gensym "from"))
					)
				    ;; because we only want to evaluate the from - to - by parameters of the for loop macro
				    ;; ONCE - we introduce a let expression at the top , then use those results in
				    ;; the macro body
				    ;; want to inject SYM because we want that to be captured 
				    `(let ((sfrom ,from)
					   (sto ,to) 		
					   (sby (abs ,by))) ;; 
				       (cond ;; for loop that goes downwards
					((< ,sto ,sfrom)
					 (letrec ((,loop (lambda (,(inject 'sym))
							   (cond
							    ((< ,(inject 'sym) ,sto) #f)
							    (#t ,@body
								(,loop (- ,(inject 'sym) ,sby)))))))
					   (,loop ,sfrom)))
					;; usual incrementing for loop
					(#t (letrec ((,loop (lambda (,(inject 'sym))
							      (cond
							       ((> ,(inject 'sym) ,sto) #f)
							       (#t ,@body
								   (,loop (+ ,(inject 'sym) ,sby)))))))
					      (,loop ,sfrom)))))))))
     (let ((pat1 '(for (sym from to by) . body))
	   (pat2 '(for (sym from to) . body)))
       (cond
	((bindable? pat1 expr)
	 (bind (for (sym from to by) . body) expr
	       (let ((loop (gensym "loop"))
					(sto (gensym "to"))
					(sby (gensym "by"))	
					(sfrom (gensym "from"))
					)
				    ;; because we only want to evaluate the from - to - by parameters of the for loop macro
				    ;; ONCE - we introduce a let expression at the top , then use those results in
				    ;; the macro body
				    ;; want to inject SYM because we want that to be captured 
				    `(let ((,sfrom ,from)
					   (,sto ,to) 		
					   (,sby (abs ,by))) ;; 
				       (cond ;; for loop that goes downwards
					((< ,sto ,sfrom)
					 (letrec ((,loop (lambda (,(inject 'sym))
							   (cond
							    ((< ,(inject 'sym) ,sto) #f)
							    (#t ,@body
								(,loop (- ,(inject 'sym) ,sby)))))))
					   (,loop ,sfrom)))
					;; usual incrementing for loop
					(#t (letrec ((,loop (lambda (,(inject 'sym))
							      (cond
							       ((> ,(inject 'sym) ,sto) #f)
							       (#t ,@body
								   (,loop (+ ,(inject 'sym) ,sby)))))))
					      (,loop ,sfrom))))))))
	((bindable? pat2)
	 (bind (for (sym from to) . body) expr
	       (let ((loop (gensym "loop"))
		     (sto (gensym "to"))
		     (by 1)	
		     (sfrom (gensym "from")))
		     
		 ;; because we only want to evaluate the from - to - by parameters of the for loop macro
		 ;; ONCE - we introduce a let expression at the top , then use those results in
		 ;; the macro body
		 ;; want to inject SYM because we want that to be captured 
		 `(let ((,sfrom ,from)
			(,sto ,to) 		
			(,sby (abs ,by))) ;; 
		    (cond ;; for loop that goes downwards
		     ((< ,sto ,sfrom)
		      (letrec ((,loop (lambda (,(inject 'sym))
					(cond
					 ((< ,(inject 'sym) ,sto) #f)
					 (#t ,@body
					     (,loop (- ,(inject 'sym) ,sby)))))))
			(,loop ,sfrom)))
		     ;; usual incrementing for loop
		     (#t (letrec ((,loop (lambda (,(inject 'sym))
					   (cond
					    ((> ,(inject 'sym) ,sto) #f)
					    (#t ,@body
						(,loop (+ ,(inject 'sym) ,sby)))))))
			   (,loop ,sfrom))))))))
	(#t (error "macro expansion FOR in for.scm IR-macro-transformer BIND failed neither pattern pat1 nor pat2 matched the expected (for (i 0 10 1) ...(format #t \"i= ~a~% \" i))"))))))))


|#







;;

;; (bindable  PAT  EXPR )

;; PAT is not quoted
;; EXPR is QUOTED

;; (let ((pat '(for (sym from to by) . body))
;;       (expr '(for (i 0 10 1) (format #t "i = ~a~%" i))))
;;   (bindable? pat expr))


;; (bindable? (for (sym from to by) . body)
;; 	   '(for (i 10 1 1)  (format #t "i = ~a~%" i)))

;; ;; if quote the PAT then get dbind errror
;; (bindable? '(for (sym from to by) . body)
;; 	   '(for (i 10 1 1)  (format #t "i = ~a~%" i)))

;; ;; a bind example
;; (bind (for (sym from to ) . body)
;;       '(for (i 10 1)  (format #t "i = ~a~%" i))
;;       (list 'sym sym 'from from 'to to 'body body))

      


;; define-macro works fine in interpreter mode but compiled does not like it attt alll
#|
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


;; (for i from 1 to 10 by -1 loops infinite ... probabyl not what wanted)
;; so have an absolute on the step 

(ppe '(for (i 1 10)  (format #t "i = ~a~%" i)))

(ppe '(down-for (i 10 1)  (format #t "i = ~a~%" i)))

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


