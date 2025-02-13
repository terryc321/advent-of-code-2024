
(import (chicken syntax))
;; the import defines er-macro-transformer

(import-for-syntax (only bindings bind bindable?))

;; 1 - does it needlesssly recompute limits start stop by multiple times
;; consider : start stop by were side effecting computations , now occuring multiple times
;; 2 - hygiene : are all
;; is hygiene not a more brittle approach ,
;; what if we truely wanted to redefine LET
;; and have everything else use that new defintion

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


	   


;; at compile time does it matter if re-bind same expression a few times again ?
(define-syntax for
  (er-macro-transformer
   (lambda (exp rename compare?)
     (let ((%letrec (rename 'letrec))
	   (%loop (rename 'loop))
	   (%lambda (rename 'lambda))
	   (%cond (rename 'cond))
	   (%stop (rename 'stop))
	   (%> (rename '>))	   
	   (%< (rename '<))	   
	   (%<= (rename '<=))
	   (%>= (rename '>=))	   
	   (%+ (rename '+))
	   (%- (rename '-))
	   (%start (rename 'start)))
       (cond
	
	((bindable? (for (var start stop) . body) exp)
	 (bind (for (var start stop) . body) exp
	       `(if (<= ,start ,stop)
		    ;; count up in 1
		    (for-helper (,var ,start ,stop ,<= ,+ ,1) ,@body)
		    
		    ;; (,%letrec
		    ;;  ((,%loop (,%lambda (,var)				  
		    ;; 			(when (,%<= ,var ,stop) 
		    ;; 			  ,@body
		    ;; 			  (,%loop (,%+ ,var 1))))))
		    ;;  (,%loop ,start))
		    ;; count down in 1
		    (,%letrec
		     ((,%loop (,%lambda (,var)
					(when (,%>= ,var ,stop) 
					  ,@body
					  (,%loop (,%- ,var 1))))))
		     (,%loop ,start)))))
	
	((bindable? (for (var start stop by) . body) exp)
	 (bind (for (var start stop by) . body) exp
	       `(if (<= ,start ,stop)
		    ;; count up by BY
		    (,%letrec
		     ((,%loop (,%lambda (,var)
					(when (,%<= ,var ,stop) 
					  ,@body
					  (,%loop (,%+ ,var ,(abs by)))))))
		     (,%loop ,start))
		    ;; count down by BY
		    (,%letrec
		     ((,%loop (,%lambda (,var)
					(when (,%>= ,var ,stop) 
					  ,@body
					  (,%loop (,%- ,var ,(abs by)))))))
		     (,%loop ,start))))))))))
