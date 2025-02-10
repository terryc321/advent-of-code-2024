
(import (chicken format))
(import (chicken pretty-print))
(import (srfi-1))
(import (simple-loops))
(define fmt format)
(import procedural-macros)
(import (chicken bitwise))
(import expand-full)
(import (srfi-69)) ;; hash tables

(import (chicken io))
(import bindings) ;;
;; (import fmt)

(import args)

(import (chicken process-context)
        (chicken port)
        args)

(require-library chicken-doc)

;; (bind (x y) '(1 2) x)
;; (bind (x y) '(1 2) y)


(define ppe (lambda (x) (pp (expand* x))))
;; %%%%%%%%% macros %%%%%%%%%%%%%%%%%%

(define-macro (incf! y)
  `(set! ,y (+ ,y 1)))


(define href/d hash-table-ref/default)
(define href hash-table-ref)
(define hset! hash-table-set!)
(define hkeys hash-table-keys)





;; (for (i from to by) ...)
;;
;; from 10 downto 1 by -2 , right think that -2 steps ,
;; do an absolute value of by then subtract or increase from to as appropriate
;;
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

#|
;; (for i from 1 to 10 by -1 loops infinite ... probabyl not what wanted)
;; so have an absolute on the step 

(ppe '(for (i 10 1)
	      (fmt #t "i = ~a~%" i)))

(for (i 3 1)
  (fmt #t "i = ~a~%" i))

(for (i 5 1 -2)
  (fmt #t "i = ~a~%" i))

(for (i 1 5 -2)
  (fmt #t "i = ~a~%" i))


(for (i 1 3)
	(fmt #t "i = ~a~%" i))

(do-for (x 1 3)
	(do-for (y 1 3)
		(fmt #t "x = ~a : y = ~a~%" x y)))


(ppe '(do-for (x 1 3)
	      (do-for (y 1 3)
		      (fmt #t "x = ~a : y = ~a~%" x y))))
     
  
(do-for (x 1 12 2)
	(fmt #t "x step in 2 = ~a ~%" x ))



(do-for (i 0 10)
	(fmt #t "i = ~a~%" i))

(do-for (i 1 3)
	(fmt #t "i = ~a~%" i))

(do-for (x 1 3)
	(do-for (y 1 3)
		(fmt #t "x = ~a : y = ~a~%" x y)))


(ppe '(do-for (x 1 3)
	      (do-for (y 1 3)
		      (fmt #t "x = ~a : y = ~a~%" x y))))
     
  
(do-for (x 1 12 2)
	(fmt #t "x step in 2 = ~a ~%" x ))

|#


;; %%%%%%%%%%%%%%%% front end %%%%%%%%%%%%%%%%%%%%%%%%%


;;; Note that missing required args can only be detected if their option appears last,
;;; and optional args must not be separated from their option by a space
;;; (e.g. -d2 or --debug=2, not -d 2 or --debug 2).


;; comment this line out when uncomment front end code below
;;(define input-filename "../example.txt")
;; (define input-filename "../input.txt")

(define input-filename #f)
(define coords-filename #f)


(define opts
 (list (args:make-option (c cookie)    #:none     "give me cookie"
         (print "cookie was tasty"))
       (args:make-option (d)           (optional: "LEVEL")  "debug level [default: 1]"
         (set! arg (string->number (or arg "1"))))
       (args:make-option (e elephant)  #:required "flatten the argument"
         (print "elephant: arg is " arg))
       (args:make-option (f file)      (required: "NAME")   "parse file NAME")
       (args:make-option (g coords)    (required: "NAME")   "parse file NAME")       
       (args:make-option (l really-long-option-name) (required: "NAME")
                         "very long option NAME to show wrapping")
       (args:make-option (v V version) #:none     "Display version"
         (print "args-example $Revision: 1.3 $")
         (exit))
       (args:make-option (abc)         #:none     "Recite the alphabet")
       (args:make-option (h help)      #:none     "Display this text"
         (usage))))

(define (usage)
 (with-output-to-port (current-error-port)
   (lambda ()
     (print "Usage: " (car (argv)) " [options...] [files...]")
     (newline)
     (print (args:usage opts))
     (print "Report bugs to zbigniewsz at gmail.")))
 (exit 1))

(receive (options operands)
    (args:parse (command-line-arguments) opts)
  ;; 'e or 'elephant both work
  (print "-e -> " (alist-ref 'elephant options))
  (print "-f -> " (alist-ref 'file options))
  (print "-g -> " (alist-ref 'coords options))
   (set! coords-filename (alist-ref 'coords options))
   (set! input-filename (alist-ref 'file options))
  )

(format #t "input-filename : ~a~%" input-filename)
(format #t "coords-filename : ~a~%" coords-filename)

(when (not coords-filename)
  (format #t "ERROR bad coords filename ~%")
  (exit 2))

(when (not input-filename)
  (format #t "ERROR bad input filename ~%")
  (exit 3))

;; (format #t "we got here.~%")
;; (exit 0)






;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; (define coords-filename "tasker1.dat")

(define coords (with-input-from-file coords-filename
		 (lambda ()
		   (read))))


;; ensure coords is a list of pairs of integers
(cond
 ((and (list? coords) (> (length coords) 1206)) #t)
 (#t (format #t "ERROR Bad coords file ~a ~%" coords-filename)
     (exit 1)))




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



(define (make-grid x y e)
  (let ((wid x)
	(hgt y)
	(rows (make-vector (+ y 3)))
	(cheat #f)
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



(define-macro (Grid.attr! g a v ) `(,g ,a ,v))
(define-macro (Grid.wid g) `(,g 'wid))
(define-macro (Grid.hgt g) `(,g 'hgt))
(define-macro (Grid.get g x y) `(,g 'get ,x ,y))
(define-macro (Grid.set g x y v) `(,g 'set ,x ,y ,v))
(define-macro (Grid.show g) `(,g 'show))
;;(define-macro (Grid.forall g) `(,g 'show))









;; (define g (make-grid 10 13 #\.))
;; (g 'wid)
;; (g 'hgt)
;; (g 'show)
;; (g 'set! 3 3 #\#)
;; (g 'set! 1 1 #\#)
;; (g 'show)
;; (define k #f)
;; ;; (g 'forall (lambda (x y read write kexit knext)
;; ;; 	     (write x y (+ x (* y (g 'hgt))))
;; ;; 	     (when (and (= x 5)(= y 8))
;; ;; 	       (set! k knext)
;; ;; 	       (kexit #f))))

;; (g 'show) 
;; ;; (k (lambda (x y read write kexit knext)
;; ;;      (write x y (+ x (* y (g 'hgt))))
;; ;;      (when (and (= x 5)(= y 8))
;; ;;        (set! k knext)
;; ;;        (kexit #f)))) ;; should continue to fill in the grid with numbers

;; (g 'show)

;; ;;(k #t)
;; (g 'show)


;; (define (grid-width g)  (vector-ref g 1))
;; (define (grid-height g)  (vector-ref g 3))
;; (define (grid-grid g)  (vector-ref g 5))
;; (define (grid-xy g x y)
;;   (let ((g (grid-grid g))
;; 	(width (grid-width g))
;; 	(height (grid-height g))
;;     (vector-ref (vector-ref g y) x)))

;; (define (show-grid g)
;;   (do-for (y 1 (+ 1 (grid-height g)))
;; 	  (fmt #t "~%")	  
;; 	  (do-for (x 1 (+ 1 (grid-width g)))
;; 		  (let ((e (grid-xy g x y)))
;; 		    (fmt #t "~a " e)))))


;; (define (test-xy)
;;   (fmt #t "width : ~a & height : ~a~%" width height)  
;;   (letrec ((loop (lambda (x y n)
;; 		   (cond
;; 		    ((> x width) (loop 1 (+ y 1) (+ n 1)))
;; 		    ((> y height) #f)
;; 		    (#t (let ((q-n (xy-to-n x y))
;; 			      (q-xy (n-to-xy n))
;; 			      (q-n-xy (n-to-xy (xy-to-n x y))))
;; 			  (fmt #t "q-n ~a : q-xy ~a : q-n-xy ~a~%" q-n q-xy q-n-xy)
;; 			  (loop (+ x 1) y (+ n 1))))))))
;;     (loop 1 1 0)))



	



;; populate grid from a list rep with strings
(define (populate xs)
  (let* ((wid (string-length (car xs)))
	 (hgt (length xs))
	 (x 1)
	 (y 1)
	 (grid (make-grid wid hgt #\.)))
    (do-list (row grid-list)
	     (let ((slen (string-length row)))
	       (for (i 0 (- slen 1))
		 (let ((ch (string-ref row i)))
		   (cond
		    ((char=? ch #\S)
		     (grid 'set! x y #\.)
		     (grid 'set-start! (list x y)))
		    ((char=? ch #\E)
		     (grid 'set! x y #\.)
		     (grid 'set-end! (list x y)))		    
		    (#t 
		     (grid 'set! x y ch)))
		   (incf! x)))
	       (incf! y)
	       (set! x 1)))
    grid))


;; playign around with different ways of set grid x y to certain square 

;; ;; generic way to handle setting and getting 
;; ;; populate grid from a list rep with strings
;; (define (populate2 xs)
;;   (let* ((wid (string-length (car xs)))
;; 	 (hgt (length xs))
;; 	 (x 1)
;; 	 (y 1)
;; 	 (g (make-grid wid hgt #\.)))
;;     (do-list (row grid-list)
;; 	     (let ((slen (string-length row)))
;; 	       (for (i 0 (- slen 1))
;; 		 (let ((ch (string-ref row i)))
;; 		   (cond
;; 		    ((char=? ch #\S)
;; 		     (Grid.set! g x y #\.)
;; 		     ;;(Grid.attr! g 'start (list x y)))
;; 		    ((char=? ch #\E)
;; 		     (Grid.set! g x y #\.)
;; 		     ;;(Grid.attr! 'end (list x y)))		    
;; 		    (#t 
;; 		     (grid 'set! x y ch)))
;; 		   (incf! x)))
;; 	       (incf! y)
;; 	       (set! x 1)))
;; 	       grid))))


;; ---------- rewrite ------------------
;; (define (forward n)
;;   (let loop ((x 1)(y 1))
;; 	(cond
;; 	 ((> x width)	  
;; 	  (loop 1 (+ y 1)))
;; 	 ((> y height) #f)
;; 	 (#t
;; 	  (let ((ch (g 'get x y)))
;; 	    (when (and (integer? ch) (= ch n))
;; 	      (go (- x 1) y (+ n 1))
;; 	      (go (+ x 1) y (+ n 1))
;; 	      (go  x (- y 1) (+ n 1))
;; 	      (go  x (+ y 1) (+ n 1)))	    
;; 	    (loop (+ x 1) y))))
;; 	(when did-set ;; use a flag to tell us if we set anything otherwise loop forever
;; 	  (set! did-set #f)
;; 	  (forward (+ n 1)))))


(define (copy-grid g)
  (let* ((start (g 'start))
	 (end (g 'end))
	 (wid (g 'wid))
	 (hgt (g 'hgt))
	 (g2 (make-grid wid hgt #\.)))
    (for (x 1 wid)
      (for (y 1 hgt)
	(let ((ch (g 'get x y)))
	  (g2 'set! x y ch))))
    (g2 'set-start! (g 'start))
    (g2 'set-end! (g 'end))
    g2))



	  
  

;; destroys the original ``grid``` with numerical values
(define (wander! g)
  (let* ((start (g 'start))
	 (end (g 'end))
	 (wid (g 'wid))
	 (hgt (g 'hgt))
	 (working '())
	 (pending (list start))
	 (did-set #f))

    ;; try put an integer at x y where there is an empty #\. char 
    (letrec ((go (lambda (x y n) 
		   (cond
		    ((<= x 1) #f)
		    ((<= y 1) #f)       
		    ((>= x wid) #f)
		    ((>= y hgt) #f)
		    (#t (let ((ch (g 'get x y)))
			  (cond
			   ((and (char? ch) (char=? ch #\.)) ;; empty square - place integer there
			    (set! did-set #t)
			    (set! pending (cons (list x y) pending))
			    (g 'set! x y n))
			   ((and (integer? ch) (< n ch)) ;; an integer here
			    ;; unlikely this ever gets executed because all values updated in one fell swoop
			    ;;(fmt #t "found better value for square ~a ~a" x y)
			    (set! pending (cons (list x y) pending))			    
			    (set! did-set #t)
			    (g 'set! x y n))))))))
	     ;; otherwise i dont care #\# wall or border of grid
	     (forward (lambda (n)

			(set! working pending)
			(set! pending '())
			;; 
			(do-list (pos working)
				 (bind (x y) pos
			
				       ;; (for (x 1 wid)
				       ;;   (for (y 1 hgt)
				       (let ((ch (g 'get x y)))
					 (when (and (integer? ch) (= ch n))
					   (go (- x 1) y (+ n 1))
					   (go (+ x 1) y (+ n 1))
					   (go  x (- y 1) (+ n 1))
					   (go  x (+ y 1) (+ n 1))))))
			
			;; use a flag to tell us if we set anything otherwise loop forever
			(when did-set 
			  (set! did-set #f)
			  (forward (+ n 1))))))
      ;; give start value of 0
      (bind (x y) start (g 'set! x y 0))
      ;; look for squares that are empty in all four directions , if so label then 1
      ;; loop again looking for squares labelled 1 and all four directions , label then 2
      ;; so on
      (forward 0)
      
      (bind (xe ye) end
	    (let ((exit (g 'get xe ye)))
	      ;;(fmt #t "exit at ~a ~%" exit)
	      exit)))))



;; ;; destroys the original ``grid``` with numerical values
;; (define (wander! g)
;;   (let* ((start (g 'start))
;; 	 (end (g 'end))
;; 	 (wid (g 'wid))
;; 	 (hgt (g 'hgt))
;; 	 (did-set #f))

;;     ;; try put an integer at x y where there is an empty #\. char 
;;     (letrec ((go (lambda (x y n) 
;; 		   (cond
;; 		    ((<= x 1) #f)
;; 		    ((<= y 1) #f)       
;; 		    ((>= x wid) #f)
;; 		    ((>= y hgt) #f)
;; 		    (#t (let ((ch (g 'get x y)))
;; 			  (cond
;; 			   ((and (char? ch) (char=? ch #\.)) ;; empty square - place integer there
;; 			    (set! did-set #t)
;; 			    (g 'set! x y n))
;; 			   ((and (integer? ch) (< n ch)) ;; an integer here
;; 			    ;; unlikely this ever gets executed because all values updated in one fell swoop
;; 			    (fmt #t "found better value for square ~a ~a" x y)
;; 			    (set! did-set #t)
;; 			    (g 'set! x y n))))))))
;; 	     ;; otherwise i dont care #\# wall or border of grid
;; 	     (forward (lambda (n)
;; 			(for (x 1 wid)
;; 			  (for (y 1 hgt)
;; 			    (let ((ch (g 'get x y)))
;; 			      (when (and (integer? ch) (= ch n))
;; 				(go (- x 1) y (+ n 1))
;; 				(go (+ x 1) y (+ n 1))
;; 				(go  x (- y 1) (+ n 1))
;; 				(go  x (+ y 1) (+ n 1))))))
;; 			;; use a flag to tell us if we set anything otherwise loop forever
;; 			(when did-set 
;; 			  (set! did-set #f)
;; 			  (forward (+ n 1))))))
;;       ;; give start value of 0
;;       (bind (x y) start (g 'set! x y 0))
;;       ;; look for squares that are empty in all four directions , if so label then 1
;;       ;; loop again looking for squares labelled 1 and all four directions , label then 2
;;       ;; so on
;;       (forward 0)
;;       (bind (xe ye) end
;; 	    (let ((exit (g 'get xe ye)))
;; 	      ;;(fmt #t "exit at ~a ~%" exit)
;; 	      exit)))))




;; so we have a global g to work from , size and 
(define g (populate grid-list))
;; (define g2 (copy-grid g))

;; (g 'show)

;; (g 'get 2 4)
;; (g 'start)
;; (g 'end)
;; (wander! g)
;; (g 'show)
;; (g2 'show)



;; if we do not need to store actually coordinates that give rise to the solution
;; we can just increment a value
;; could parallelize this program by having each program take its own data set to work on 
;; no communication , totally seperate processes , true parallelism 
;; then when all processes have completed
;; we can look at the output of the programs
;;
;; need a program to figure out the distribution of the work to different programs
;; how will they combine their totals ?
;; do we just need a number over 100 picoseconds ,
;; any cheat square that scores over a 100 picoseconds saved gets added to the total
;; so entire program will just produce a single value as result
;; represents number of cheat squares that produced a saving of 100 picoseconds or more.
;; simply need to sum up these values 
;; (define (cheat gold)			;
;;   (let ((start (g 'start))
;; 	(end (g 'end))
;; 	(wid (g 'wid))
;; 	(hgt (g 'hgt))
;; 	(nominal (wander! (copy-grid gold)))
;; 	;;(hash (make-hash-table))
;; 	(tot 0))
;;     (for (cx 2 (- wid 1))
;;       (for (cy 2 (- hgt 1))
;; 	(let ((g (copy-grid gold)))
;; 	  ;; copy grid ?
;; 	  ;; place cheat at cx cy - an empty square
;; 	  (g 'set! cx cy #\.)
;; 	  ;; wander
;; 	  (let* ((exit (wander! g))
;; 		 (save (- nominal exit))
;; 		 (pos (list cx cy)))
;; 	    ;; only save if we actually saved some time
;; 	    (when (> save 0)
;; 	      (incf! tot))))))
    
;; 	    ;; ;; store location cx cy under savings key
;; 	    ;; (let ((hv (href/d hash save #f)))
;; 	    ;;   (cond
;; 	    ;;    ((eq? hv #f) (hset! hash save (list pos)))
;; 	    ;;    ((member pos hv) #f)
;; 	    ;;    (#t (hset! hash save (cons pos hv))))))))))
    
;;     ;; for each - list them out in some order?
;;     ;; (do-list (save (hkeys hash))
;;     ;; 	     (let ((coords (href hash save)))
;;     ;; 	       (format #t "there are ~a solutions saving ~a picoseconds~%" (length coords) save)))
;;     (format #t "total number of squares that resulted in a saving were ~a~%" tot)
;;     tot))


(define (cheat gold coords)
  (let ((start (g 'start))
	(end (g 'end))
	(wid (g 'wid))
	(hgt (g 'hgt))
	(nominal (begin
		   (format #t "wandering original grid for nominal value : ")
		   (let ((sol (wander! (copy-grid gold))))
		     (format #t "~a discovered~%" gold)
		   sol)))
	;;(hash (make-hash-table))
	(hash2 (make-hash-table))
	(coords-len (length coords))
	(coords-i 0)
	(tot 0))
    (format #t "starting processing ...~%")
    ;; (for (cx 2 (- wid 1))
    ;;   (for (cy 2 (- hgt 1))
    (do-list (pos coords)
	     (incf! coords-i)
	     (format #t "~a / ~a : processing square ~a ~%~!" coords-i coords-len pos)
	     (bind (cx cy) pos
		   (let ((g (copy-grid gold)))
		     ;; copy grid ?
		     ;; place cheat at cx cy - an empty square
		     (g 'set! cx cy #\.)
		     ;; wander
		     (let* ((exit (wander! g))
			    (save (- nominal exit))
			    (pos (list cx cy)))
		       ;; only save if we actually saved some time
		       ;; when we save atleast 100 picoseconds of time 
		       (when (>= save 100)
			 
			 ;; store location cx cy under savings key
			 (let ((hv (href/d hash2 save #f)))
			   (cond
			    ((eq? hv #f)
			     ;;(hset! hash save (list pos))
			     (hset! hash2 save 1)
			     )
			    ;; ((member pos hv)
			    ;; 	(format #t "this square already counted ~%")
			    ;; 	#f)
			    (#t
			     ;;(hset! hash save (cons pos hv))
			     (hset! hash2 save (+ 1 (href hash2 save)))
			     ))))))))
    
    ;; for each - list them out in some order?
    (set! tot 0)
    (do-list (save (hkeys hash2))
	     (let* (;;(coords (href hash save))
		    ;;(len (length coords))
		    (len2 (href hash2 save)))
	       (set! tot (+ tot len2))	       
	       ;;(format #t "there are ~a : ~a solutions saving ~a picoseconds~%" len len2 save)
	       (format #t "there are ~a solutions saving ~a picoseconds~%" len2 save)
	       ))
    (format #t "~% in total there are ~a solutions ~%" tot)
    tot))





	  

(define (part-1)
  (format #t "populating the grid list ~%")
  (let ((g (populate grid-list)))
    ;; read a list of coordinates to cheat on
    ;; coords - read from command line filename 
    (format #t "proceeding to cheat given coords ~%")
    ;;(flush-output-port #t)
    (cheat g coords)))


;; %%%%%%%%%%%%%%% WORK HORSE %%%%%%%%%%%%%
(part-1)
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;     assigned the work for 16 processes
;;  runnign from tasker1.dat .... tasker16.dat 
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; 141 x 141 square split work up into 16 equal regions
(define (assign-work)
  (let* ((start (g 'start))
	 (end (g 'end))
	 (wid (g 'wid))
	 (hgt (g 'hgt))
	 (n-states 0)
	 (nprocs 16) ;; tried 16 all processes got killed ? %%%%%%%%% SET PROCESSOR COUNT  HERE 
	 (tasks (make-vector (+ nprocs 2) '()))
	 (task-i 1))

    (for (x 2 (- wid 1))
      (for (y 2 (- hgt 1))
	(incf! n-states)))

    (format #t "there are ~a states to explore~%" n-states)

    ;; task-i index into task vector 1 to nprocs inclusive 
    ;;(set! task-i 1)
    ;; one way would be have an index moving through a vector of 16 tasks
    ;; adding that state to task
    (for (x 2 (- wid 1))
      (for (y 2 (- hgt 1))
	(let ((pos (list x y)))	   	  
	  (let ((task-list (vector-ref tasks task-i)))	    
	    ;; for this tasker
	    ;;(when (= task-i tasker)
	      (vector-set! tasks task-i (cons pos task-list))
	      (format #t "assigning ~a to tasker ~a~%" pos task-i)
	    
	    ;; rolls round to next tasker
	    (incf! task-i)		
	    (when (> task-i nprocs)
	      (set! task-i 1))))))
    
    ;; (for (tasker 1 nprocs 1)
    ;;   (let ((outfile (format #f "tasker~a.dat" tasker)))
    ;; 	(with-output-to-file outfile
    ;; 	  (lambda ()
    ;; 	    (format #t " tasker ~a ~%" tasker)


    (let ((tot 0))
      (for (tasker-i 1 nprocs)
	(let ((task-list (vector-ref tasks tasker-i)))
	  (format #t "tasker ~a has ~a tasks ~%" tasker-i (length task-list))
	  (let ((outfile (format #f "tasker~a.dat" tasker-i)))
	    (with-output-to-file outfile
	      (lambda ()
		(pp task-list))))
	  (set! tot (+ tot (length task-list)))))
      tot)))











;; %%%%%%%%%%%%%%%%%%% buggy code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;
;; expected
;;
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; ;; 141 x 141 square split work up into 16 equal regions
;; (define (assign-work)
;;   (let* ((start (g 'start))
;; 	 (end (g 'end))
;; 	 (wid (g 'wid))
;; 	 (hgt (g 'hgt))
;; 	 (n-states 0)
;; 	 (nprocs 16)
;; 	 (tasks (make-vector (+ nprocs 2) '()))
;; 	 (task-i 1))

;;     (for (x 2 (- wid 1))
;;       (for (y 2 (- hgt 1))
;; 	(incf! n-states)))

;;     (format #t "there are ~a states to explore~%" n-states)

;;     (for (tasker 1 nprocs 1)
;;       (format #t " tasker ~a ~%" tasker)
;;       ;; task-i index into task vector 1 to nprocs inclusive 
;;       (set! task-i 1)
;;       ;; one way would be have an index moving through a vector of 16 tasks
;;       ;; adding that state to task
;;       (for (x 2 (- wid 1))
;; 	(for (y 2 (- hgt 1))
;; 	  (let ((pos (list x y)))
;; 	    (when (= task-i tasker)
	      
;; 	      (let ((task-list (vector-ref tasks task-i)))
;; 		(vector-set! tasks task-i (cons pos task-list))
;; 		(format #t "assigning ~a to tasker ~a" pos task-i)
		
;; 		;; rolls round to next tasker
;; 		(incf! task-i)		
;; 		(when (> task-i nprocs)
;; 		  (set! task-i 1))))))))

    
;;     (let ((tot 0))
;;       (for (i 1 nprocs)
;; 	(let ((task-list (vector-ref tasks i)))
;; 	  (format #t "tasker ~a has ~a tasks ~%" i (length task-list))
;; 	  (set! tot (+ tot (length task-list)))))
;;       tot)))







;; 169 states - suppose we split this off into 16 tasks
;; how many states is that per task ?
;; (modulo 169 16) = 9
;; (floor (/ 169 16)) = 10
;;    each of the 16 tasks has 10 states = 160 states covered
;;        9 states left over
;; breakdown overall :
;;        9 tasks will get 11 states to cover
;;        7 tasks will get 10 states to cover
;; evenly distributed amongst task


#|

==> tasker1.out <==
 in total there are 124 solutions

==> tasker2.out <==
 in total there are 230 solutions

==> tasker3.out <==
 in total there are 93 solutions

==> tasker4.out <==
 in total there are 234 solutions

==> tasker5.out <==
 in total there are 100 solutions

==> tasker6.out <==
 in total there are 245 solutions

==> tasker7.out <==
 in total there are 84 solutions

==> tasker8.out <==
 in total there are 248 solutions

(+ 248 84 245 100 234 93 230 124)   1358 ???

real	1m23.499s
user	8m46.490s
sys	0m2.827s

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

time ./run4.sh

==> tasker1.out <==
 in total there are 224 solutions 

==> tasker2.out <==
 in total there are 475 solutions 

==> tasker3.out <==
 in total there are 177 solutions 

==> tasker4.out <==
 in total there are 482 solutions 

rebuilding multi program in case source file changes
launching 4 tasks to solve puzzle 

real	1m53.340s
user	7m21.708s
sys	0m2.535s

(+ 482 177 475 224)  1358


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

==> tasker10.out <==
there are 3 solutions saving 240 picoseconds

 in total there are 174 solutions 

==> tasker11.out <==
there are 1 solutions saving 2082 picoseconds

 in total there are 55 solutions 

==> tasker12.out <==
there are 1 solutions saving 240 picoseconds

 in total there are 152 solutions 

==> tasker13.out <==

==> tasker14.out <==

==> tasker15.out <==

==> tasker16.out <==

==> tasker1.out <==
there are 1 solutions saving 242 picoseconds

 in total there are 67 solutions 

==> tasker2.out <==
there are 1 solutions saving 7244 picoseconds

 in total there are 147 solutions 

==> tasker3.out <==
there are 1 solutions saving 7242 picoseconds

 in total there are 64 solutions 

==> tasker4.out <==
there are 2 solutions saving 240 picoseconds

 in total there are 158 solutions 

==> tasker5.out <==
there are 1 solutions saving 1570 picoseconds

 in total there are 81 solutions 

==> tasker6.out <==
there are 2 solutions saving 240 picoseconds

 in total there are 154 solutions 

==> tasker7.out <==
there are 1 solutions saving 242 picoseconds

 in total there are 58 solutions 

==> tasker8.out <==
there are 2 solutions saving 240 picoseconds

 in total there are 172 solutions 

==> tasker9.out <==
there are 1 solutions saving 242 picoseconds

 in total there are 76 solutions 

(+ 76 172 58 154 81 158 64 147 67 152 55 174)  1358



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


terry@debian:~/code/advent-of-code/advent-of-code-2024/day20/chicken$ time ./run16.sh
rebuilding multi program in case source file changes
launching 16 tasks to solve puzzle 

real	1m11.838s
user	15m47.821s
sys	0m5.315s

==> tasker10.out <==
 in total there are 110 solutions

==> tasker11.out <==
 in total there are 42 solutions

==> tasker12.out <==
 in total there are 121 solutions

==> tasker13.out <==
 in total there are 55 solutions

==> tasker14.out <==
 in total there are 119 solutions

==> tasker15.out <==
 in total there are 45 solutions

==> tasker16.out <==
 in total there are 132 solutions

==> tasker1.out <==
 in total there are 63 solutions

==> tasker2.out <==
 in total there are 120 solutions

==> tasker3.out <==
 in total there are 51 solutions

==> tasker4.out <==
 in total there are 113 solutions

==> tasker5.out <==
 in total there are 45 solutions

==> tasker6.out <==
 in total there are 126 solutions

==> tasker7.out <==
 in total there are 39 solutions

==> tasker8.out <==
 in total there are 116 solutions

==> tasker9.out <==
 in total there are 61 solutions
(+ 110 42 121 55 119 45 132 63 120 51 113 45 126 39 116) 1297

possibly there were more entries on screen and my copy paste failed me 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

==> tasker10.out <==
there are 1 solutions saving 492 picoseconds

 in total there are 110 solutions 

==> tasker11.out <==
there are 1 solutions saving 162 picoseconds

 in total there are 42 solutions 

==> tasker12.out <==
there are 1 solutions saving 492 picoseconds

 in total there are 121 solutions 

==> tasker13.out <==
there are 1 solutions saving 178 picoseconds

 in total there are 55 solutions 

==> tasker14.out <==
there are 1 solutions saving 596 picoseconds

 in total there are 119 solutions 

==> tasker15.out <==
there are 2 solutions saving 178 picoseconds

 in total there are 45 solutions 

==> tasker16.out <==
there are 2 solutions saving 176 picoseconds

 in total there are 132 solutions 

==> tasker1.out <==
there are 1 solutions saving 178 picoseconds

 in total there are 63 solutions 

==> tasker2.out <==
there are 1 solutions saving 176 picoseconds

 in total there are 120 solutions 

==> tasker3.out <==
there are 1 solutions saving 182 picoseconds

 in total there are 51 solutions 

==> tasker4.out <==
there are 1 solutions saving 596 picoseconds

 in total there are 113 solutions 

==> tasker5.out <==
there are 1 solutions saving 178 picoseconds

 in total there are 45 solutions 

==> tasker6.out <==
there are 2 solutions saving 480 picoseconds

 in total there are 126 solutions 

==> tasker7.out <==
there are 1 solutions saving 482 picoseconds

 in total there are 39 solutions 

==> tasker8.out <==
there are 1 solutions saving 176 picoseconds

 in total there are 116 solutions 

==> tasker9.out <==
there are 1 solutions saving 182 picoseconds

 in total there are 61 solutions 

(+ 61 116  39 126 45 113 51 120 63 132 45 119 55 121 42 110)  1358

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


suggested solution is 1358

***********************************



|#
    
