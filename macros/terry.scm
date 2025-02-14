
(module terry (with-symbols for-helper)

(import bindings)
(import scheme)
(import (chicken syntax))
(import (chicken format))
(import-for-syntax scheme)
(import-for-syntax (chicken format))

(import-for-syntax bindings)

(begin-for-syntax
;; functions that want to have at compile time , need to be enclosed in begin-for-syntax 
 
;; takes list of names and produces %name for each name
(define percent-symbol
  (lambda (s)
    (format #t "s => [~a]~%" s)
    (string->symbol (string-append "%" (symbol->string s)))))

(define symbols->percents
  (lambda (xs)
    (map (lambda (x)
	   `(,(percent-symbol x) (rename ',x)))
	 xs)))

)


(define-syntax for-helper
  (er-macro-transformer
   (lambda (exp rename compare?)
     (let ((%loop (rename 'loop))
	   (%lambda (rename 'lambda))	   
	   (%when (rename 'when)))
       (bind (_ (var start stop cmp inc by) . body) exp	   
	     `(letrec
		  ((,%loop (,%lambda (,var)				  
				     (,%when (,cmp ,var ,stop) 
				       ,@body
				       (,%loop (,inc ,var ,by))))))
		(,%loop ,start)))))))






;; (with-symbols (the symbols) . . . )
(define-syntax with-symbols
  (er-macro-transformer
   (lambda (exp rename compare?)
     (let* ((symbols (car (cdr exp)))
	    (body (cdr (cdr exp)))
	    (rename-list (symbols->percents symbols)))
       
       ;;`'(,rename-list)
       `(let ,rename-list ,@body)
       
       ))))



) ;; module 
