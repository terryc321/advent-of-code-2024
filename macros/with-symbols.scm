
(module terry (with-symbols)

(import (chicken syntax))
(import (chicken format))
(import-for-syntax scheme)
(import-for-syntax (chicken format))

(begin-for-syntax 
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


)
) ;; module 
