
;; requires FOR macro 
;; ,l for.scm

;;(load "for.scm")
;;(load "dolist")


#|

some sample code on how to 

(define input-filename "../input.txt")
;;(define input-filename "../example.txt")


;; open the file input.txt
;; read a set of lines to represent the grid
;; read all the grids
;; close the file
;; now have grids
(define (empty-line? x)
  (equal? x ""))

(define (split-lines xs)
  (letrec ((foo (lambda (xs acc res)
		  (cond
		   ((null? xs) (cond
				((null? acc) res)
				(#t (cons (reverse acc) res))))
		   ((empty-line? (car xs))
		    (cond
		     ((null? acc) (foo (cdr xs) '() res))
		     (#t (foo (cdr xs) '() (cons (reverse acc) res)))))
		   (#t (foo (cdr xs) (cons (car xs) acc) res))))))
    (let ((out (foo xs '() '())))
      (reverse out))))


(define lines (with-input-from-file input-filename
		(lambda ()
		  (read-lines))))

(define grid-list (car (split-lines lines)))

;; (define width (string-length (car grid-list))) ;; how many columns 
;; (define height (length grid-list));; how many rows

;; (define width 10)
;; (define height 10)


;; ;; for each element of grid

;; (define grid (make-vector (* (+ width 2) (+ height 2))))
|#

;; (module grid (make-grid)
;;   (import scheme)
;;   (import )

(define (make-grid x y e)
  (let ((wid x)
	(hgt y)
	(rows (make-vector (+ y 3)))
	(cheat #f) ;; ?? 
	(start #f)
	(end #f)
	(this #f))
    (for (i 1 y)
	    (vector-set! rows i (make-vector (+ x 3) e)))
    (letrec ((unsafe-get (lambda (x y) ;; no checks
			   (vector-ref (vector-ref rows y) x)))
	     (unsafe-set (lambda (x y v) ;; no checks
			   (vector-set! (vector-ref rows y) x v)))
	     (get (lambda (args) ;; x y with checks
		    (bind (x y) args
			  (cond
			   ((< x 1) (error "get x < 1 "))
			   ((> x wid) (error "get x > width"))
			   ((< y 1) (error "get y < 1"))
			   ((> y hgt) (error "get y > height"))
			   (#t (unsafe-get x y))))))
	     (set (lambda (args) ;; x y value with checks
		    (bind (x y v) args 
			  (cond
			   ((< x 1) (error "set! x < 1 "))
			   ((> x wid) (error "set! x > width"))
			   ((< y 1) (error "set! y < 1 "))
			   ((> y hgt) (error "set! y > height"))
			   (#t (unsafe-set x y v))))))
	     (copy (lambda ()
		     (let ((copy (make-grid x y e)))
		       (for (y 1 hgt)
			    (for (x 1 wid)
				 (let ((e (unsafe-get x y)))
				   (copy 'set! x y e))))
		       copy)))
	     (show (lambda () ;; square and we control the x y , opted for no checks
		     (fmt #t "~%")
		     (fmt #t "start at ~a : end at ~a ~%" start end)
		     (for (y 1 hgt)
		       (fmt #t "~%")	  
		       (for (x 1 wid)
			 (let ((pos (list x y)))
			 (cond
			  ((equal? cheat pos)
			   (fmt #t "* "))
			  ((equal? start pos)
			   (fmt #t "S "))
			  ((equal? end pos)
			   (fmt #t "E "))
			  (#t
			   (let ((e (unsafe-get x y)))
			     (fmt #t "~a " e)))))))))

	     ;; surely something we can do with this 
	     (forall (lambda (args) ;; square and we control the x y , opted for no checks
		       (bind (fn) args
			     (call/cc (lambda (exit) ;; fn takes x y read write kont 
					(for (y 1 hgt)			 
					  (for (x 1 wid)
					    (set! fn (call/cc (lambda (next)
								(fn x y unsafe-get unsafe-set exit next)
								fn))))))))))
	     (set-start! (lambda (args) (bind (s) args (set! start s))))
	     (set-end! (lambda (args) (bind (s) args (set! end s))))
	     (start (lambda (args) start))
	     (end (lambda (args) end))
	     
	     (set-cheat! (lambda (args) (bind (s) args (set! cheat s))))
	     (cheat (lambda (args) cheat))
	     
	     )
      (set! this
	(lambda (op . args)
	  (cond
	   ;; ((eq? op 'attr) (attr args)) ;; attributes 
	   ;; ((eq? op 'attr!) (attr! args))
	   ((eq? op 'copy) (copy))
	   ((eq? op 'wid) wid)
	   ((eq? op 'hgt) hgt)
	   ((eq? op 'data) rows)
	   ((eq? op 'get) (get args))
	   ((eq? op 'set!) (set args))
	   
	   ((eq? op 'start) start)
	   ((eq? op 'set-start!) (set-start! args))

	   ((eq? op 'end) end)
	   ((eq? op 'set-end!) (set-end! args))

	   ((eq? op 'set-cheat!) (set-cheat! args))
	   ((eq? op 'cheat) cheat)	   
	   
	   ((eq? op 'show) (show))
	   ;; ((eq? op 'forall) (forall args))       
	   (#t (error (format #f "unknown message [~a] to make-grid" op))))))
      this)))



;; (define-macro (Grid.attr! g a v ) `(,g ,a ,v))
;; (define-macro (Grid.wid g) `(,g 'wid))
;; (define-macro (Grid.hgt g) `(,g 'hgt))
;; (define-macro (Grid.get g x y) `(,g 'get ,x ,y))
;; (define-macro (Grid.set g x y v) `(,g 'set! ,x ,y ,v))
;; (define-macro (Grid.show g) `(,g 'show))
;; ;;(define-macro (Grid.forall g) `(,g 'show))
;;)

