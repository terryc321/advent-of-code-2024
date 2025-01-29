
;; have access to file location 
(define foo (current-filename))

;; ;; with-open-file 
;; (let ((port (open-input-file "foo.txt"))) 
;;   (display (read-line port)) (newline) (close-port port))
;; =======================================================================

;; guile

;; read lilnes from a file
(use-modules (ice-9 rdelim))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 readline))
;;(use-modules (ice-9 assert))  no assert module huh 
(define pp pretty-print)
(use-modules (ice-9 format))
(define fmt format)

;; srfi-42
(use-modules (srfi srfi-42))

;; basic list stuff
(use-modules (srfi srfi-1))

;; guile has a weird thing where it wants to be in parent directory when open emacs 
;;https://www.scheme.com/tspl3/io.html
(define read-example 
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p) ;; p port
	;; f recursive
	(let ((lines '()))
	  (letrec ((rl (lambda (line)
			 (let ((ch (read-char p)))
			   (cond
			    ((eof-object? ch)
			     (cond
			      ((null? line) (set! lines (reverse lines)))
			      (#t (set! lines (reverse (cons (reverse line) lines))))))
			    ((char=? ch #\newline)
			     (set! lines (cons (reverse line) lines))
			     (rl '()))			     
			    (#t (rl (cons ch line))))))))
	    (rl '())
	    lines))))))

(define example1 (read-example "../example1.txt"))
(define example2 (read-example "../example2.txt"))
(define input1 (read-example "../input.txt"))

;; keep taking until nil  
(define find-grid
  (lambda (xs)
    (cond
     ((null? xs) xs)
     (#t (let ((elem (car xs)))
	   (cond
	    ((null? elem) '())
	    (#t (cons elem (find-grid (cdr xs))))))))))


;;(to-array (find-grid example2))
(define to-array
  (lambda (xs)
    (let* ((hgt (length xs))
	   (wid (length (car xs)))
	   (grid (make-vector hgt 0))
	   (rows xs)
	   (cols #f))
      (fmt #t "wid hgt grid rows cols ~a ~a ~a ~a ~a ~%" wid hgt grid rows cols)
      (let loop ((y 0))
	(cond
	 ((< y hgt)
	  (set! cols (car rows))
	  (set! rows (cdr rows))
	  (let ((tmp (make-vector wid #\#)))
	  (fmt #t "tmp ~a : y ~a ~%" tmp y)
	  (vector-set! grid y tmp)
	  (let loop2 ((x 0))
	    (cond
	     ((< x wid)
	      (vector-set! tmp x (car cols))
	      (set! cols (cdr cols))
	      (loop2 (+ x 1)))))	     
	  (loop (+ y 1))))
	 ))
      ;; compound grid type
      `((wid ,wid)(hgt ,hgt)(grid ,grid)))))
;;(moves ,(apply append (cdr rows)))))))

(define internal-xy
  (lambda (ty x y)
    (let ((g (car (cdr (assoc 'grid ty)))))
      (vector-ref (vector-ref g y) x))))

(define internal-xy!
  (lambda (ty x y z)
    (let ((g (car (cdr (assoc 'grid ty)))))
      (vector-set! (vector-ref g y) x z))))

      


(define g (to-array (find-grid example2)))
(internal-xy g 2 2)
(internal-xy g 2 4)
(internal-xy g 3 1)
(internal-xy! g 3 1 #\t)
(pp g )
(internal-xy! g 0 0 #\s)
(pp g )



