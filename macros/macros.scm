
#|				 
antlr.scm
doc.scm
expand.scm
for.scm
grid.scm
hash.scm
hello.scm
import.scm
incf.scm
load.scm
modules
readme
reliability.scm
swap.scm
|#


(module macros (swap!
		incf!
		foo
		for
		format
		)
  (import scheme)
  (import procedural-macros)
  ;;(import bindings)
  (import (chicken format))
  ;;(import-for-syntax scheme)


  ;; ok 
  ;; (swap! x y)
  (define-syntax swap!
    (er-macro-transformer
     (lambda (exp rename compare)
       (let ((%x (car (cdr exp)))
	     (%y (car (cdr (cdr exp))))
	     (%tmp (rename 'tmp))
	     (%let (rename 'let))
	     (%set! (rename 'set!)))
	 `(,%let ((,%tmp ,%x))
		 (,%set! ,%x ,%y)
		 (,%set! ,%y ,%tmp))))))
  
  
    
  ;; (define-macro (swap! x y) 
  ;;   `(let ((tmp ,x))
  ;;      (set! ,x ,y)
  ;;      (set! ,y tmp)))

  ;; foo
  ;;(define foo (lambda () (current-module)))
  (define (foo) ;; (current-module)
    123)

  ;; define macro does not destructure under compilation 
  ;; (define-macro (incf! x)
  ;; `(set! ,x (+ ,x 1)))
  (define-syntax incf!
    (er-macro-transformer
     (lambda (exp rename compare)
       (let ((x (car (cdr exp)))
	     (%set! (rename 'set!))
	     (%+ (rename '+)))
	 `(,%set! ,x (,%+ ,x 1))))))
  

  ;; here with implicit renaming , we avoid a lot of unneccesary renaming
  ;; if we happen to miss one , its renamed automatically
  ;; in order to capture variable exit we must inject exirt into expression
  ;;  ,(inject 'exit)
  ;; do we need gensyms on implicit-renaming transformer ??
  ;; expression differes from define-macro becuase expr represents entire expression
  ;; no just expression args given to for macro like in efine macro
  (define-syntax for
    (ir-macro-transformer
     (lambda (expr inject compare)
       (let (      (sym (car (cdr expr)))
	     (from (car (cdr (cdr expr))))
	  (to (car (cdr (cdr (cdr expr)))))
	  (by (if (null? (cdr (cdr (cdr (cdr expr)))))
		     1
		     (car (cdr (cdr (cdr (cdr expr)))))))
	     ;; (loop (gensym "loop"))
	     ;; (sto (gensym "to"))
	     ;; (sby (gensym "by"))	
	     ;; (sfrom (gensym "from"))
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
|#

  
  );; module
