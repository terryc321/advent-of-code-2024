
(module for-tidy (for)

(import scheme) 
(import (chicken syntax))
;; the import defines er-macro-transformer

(import bindings)
(import-for-syntax (only bindings bind bindable?))

(import (chicken format))
(import-for-syntax (chicken format))

(import srfi-1)
(import-for-syntax srfi-1)

;; my with-symbols
(import-for-syntax terry)
(import terry)

;; 1 - does it needlesssly recompute limits start stop by multiple times
;; consider : start stop by were side effecting computations , now occuring multiple times
;; 2 - hygiene : are all
;; is hygiene not a more brittle approach ,
;; what if we truely wanted to redefine LET
;; and have everything else use that new defintion


;; the helper provided with most need to write the helper , only a few external words
;; lambda when - need to be protected from redefintion
;; need a name for the loop procedure - rename does this also , like gensym 
;; essentially all there is for this .

 
;; (define-syntax for-helper
;;   (er-macro-transformer
;;    (lambda (exp rename compare?)
;;      (let ((%loop (rename 'loop))
;; 	   (%lambda (rename 'lambda))	   
;; 	   (%when (rename 'when)))
;;        (bind (_ (var start stop cmp inc by) . body) exp	   
;; 	     `(letrec
;; 		  ((,%loop (,%lambda (,var)				  
;; 				     (,%when (,cmp ,var ,stop) 
;; 				       ,@body
;; 				       (,%loop (,inc ,var ,by))))))
;; 		(,%loop ,start)))))))




       ;;`'(let (,rename-list) ,@body)))))
       ;;`',(symbols->percents symbols)))))
;; `(let (,(symbols->percents symbols)) ,@body)))))

;; cant have
;; (a-macro (letrec foo bar baz) ..)
;; as macro expander will see letrec and expand that , rather than 


;; macro -> produces an s expression tree : not a result not at runtime dorothy
;; if write a macro and want another macro to expand it , that result must be an s expression
;; then let the macro transformer do its thing
(define simple-helper
  (lambda (exp rename compare?)
    (let* ((decl (second exp)))
      decl)))


;; `(let ,(symbols->percents '(letrec loop lambda cond stop > < <= >= + - abs start))
;;    (cond

;; 	((bindable? (for (var start stop) . body) ,exp)
;; 	 (bind (for (var start stop) . body) ,exp
;; 	       (if (<= ,start ,stop)
;; 		    ;; count up in 1 or count down in 1 
;; 		    (for-helper (,var ,start ,stop ,%<= ,%+ ,1) ,@body)
;; 		    (for-helper (,var ,start ,stop ,%>= ,%- ,1) ,@body))))

;; 	((bindable? (for (var start stop by) . body) ,exp)
;; 	 (bind (for (var start stop by) . body) ,exp
;; 	       (if (<= ,start ,stop)
;; 		    (for-helper (,var ,start ,stop ,%<= ,%+ ,by) ,@body)
;; 		    (for-helper (,var ,start ,stop ,%>= ,%- ,by) ,@body)))))))))



;;(use-for-syntax with-symbols)

(define-syntax for2
  (er-macro-transformer
   (lambda (exp rename compare?)     
     (simple-helper exp rename compare?))))

;; (let ((rename (lambda (x) #t)))
;;   (with-symbols (a b c) 1 2 3))

;; at compile time does it matter if re-bind same expression a few times again ?
(define-syntax for
  (er-macro-transformer
   (lambda (exp rename compare?)

     (with-symbols (dummy letrec loop lambda cond > < >= <= + - start stop by abs)
		   
     ;; (let ((%letrec (rename 'letrec))
     ;; 	   (%loop (rename 'loop))
     ;; 	   (%lambda (rename 'lambda))
     ;; 	   (%cond (rename 'cond))
     ;; 	   (%> (rename '>))
     ;; 	   (%< (rename '<))
     ;; 	   (%<= (rename '<=))
     ;; 	   (%>= (rename '>=))	   
     ;; 	   (%+ (rename '+))
     ;; 	   (%- (rename '-))
     ;; 	   (%start (rename 'start))
     ;; 	   (%stop (rename 'stop))
     ;; 	   (%by (rename 'by))
     ;; 	   (%abs (rename 'abs)))
       
       (cond
	
	((bindable? (for (var start stop) . body) exp)
	 (bind (for (var start stop) . body) exp
	       `(let ((,%start ,start)
		      (,%stop ,stop))
		  (if (<= ,%start ,%stop)
		    ;; count up in 1 or count down in 1 
		    (for-helper (,var ,%start ,%stop ,%<= ,%+ ,1) ,@body)
		    (for-helper (,var ,%start ,%stop ,%>= ,%- ,1) ,@body)))))
	
	((bindable? (for (var start stop by) . body) exp)
	 (bind (for (var start stop by) . body) exp
	       `(let ((,%start ,start)
		      (,%stop ,stop)
		      (,%by ,by))		      
		  (if (<= ,%start ,%stop)
		      (for-helper (,var ,%start ,%stop ,%<= ,%+ ,%by) ,@body)
		      (for-helper (,var ,%start ,%stop ,%>= ,%- ,%by) ,@body))))))))))



		    
;; (for (i 1 10 -2) (write "i=")(write i) (newline))
;; did author intend to write this ??
;; should it stop if it falls outside the range of 1 to 10 ?
;; should error be generated ?
;; what does it even mean ?

;; can we have named arguments instead
;; (for (i :from 10 :to 12 :by 3) ... )

) ;; module 
