
;; chicken scheme - day 1 advent of code 2024 
(import scheme)
(import (chicken format))
(import (chicken sort))

;; assertion library ?
;;(import assertion)


;; how do i read in a file of numbers and split them 
(define input
  (with-input-from-file "../input.txt"
    (lambda ()
      (read))))

(sort '(1 2 3 2 1 ) <)
(sort '(1 2 3 2 1 ) >)

(define test-input '(3   4
                     4   3
                     2   5
                     1   3
                     3   9
                     3   3))


(define lefts 
  (lambda (x)
    #t))






#|
(define (lefts x)
  (cond
   ((null? x) '())
   (#t (cons (car x) (lefts (cdr (cdr x)))))))

(define (rights x)
  (define (rights-helper x)
    ;;(format #t "x = ~a~%" x)
     (cond
     ((null? x) '())     
     (#t (cons (car x) (or (null? (cdr x)) 
                           '()
                           (rights-helper (cdr (cdr x))))))))
  (rights-helper (cdr x)))
|#













#|
(define split 
  (let ((a '())
        (b '()))       
    (lambda (x)
      (cond
       ((null? x) (values a b))
       (#t (let ((a0 (car x))
                 (b0 (car (cdr x))))
             (set! a (cons a0 a))
             (set! b (cons b0 b))
                       (split (cdr (cdr x)))))))))
(define split-test-input (lambda () (split test-input))
  (call-with-values (lambda (a b)
                      (format #t "a = ~a~%" a)
                      (format #t "b = ~a~%" b))
    ))


(call-with-values (lambda (a b) (list a b)) (values 1 2))
(call-with-values 
    (lambda () (values 1 2)) 
  (lambda (a b) (list a b)))


;;input

(sort '(1 2 3 2 1 ) #'<)

(defvar p '(3   4
4   3
2   5
1   3
3   9
3   3))

;;split them 
(defun split (s)  
(let ((a (car s))
(b (cadr s)))
(split (cddr s))))

(split p)
|#




