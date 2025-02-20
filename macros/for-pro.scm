
;; bit of a let down , cannot use it properly in modules


(module for-pro (for)

(import scheme) 
;;(import (chicken syntax))
;; ;; the import defines er-macro-transformer

;;(import bindings)
(import-for-syntax (only bindings bind bindable?))

;;(import (chicken format))
;; (import-for-syntax (chicken format))

;; (import srfi-1)
;;(import-for-syntax srfi-1)

;; ;; my with-symbols
(import-for-syntax terry)
;;(import terry)

(define-syntax for
  (er-macro-transformer
   (lambda (exp rename compare?)
     (with-symbols (dummy letrec loop lambda cond > < >= <= + - start stop by abs)
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

) ;; module 
