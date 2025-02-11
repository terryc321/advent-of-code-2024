

;; expanding macros 
(import expand-full) ;; expand* macro expansion
(define ppe (lambda (x) (pp (expand* x)))) ;; pretty-print macro expanded fully expression x 


