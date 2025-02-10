
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


(define (cheat gold)
  (let* ((start (g 'start))
	(end (g 'end))
	(wid (g 'wid))
	(hgt (g 'hgt))
	(nominal (wander! (copy-grid gold)))
	;;(hash (make-hash-table))
	(hash2 (make-hash-table))
	(step 0)
	(total-steps (* (- wid 1)(- hgt 1)))
	(tot 0))
    (for (cx 2 (- wid 1))
      (for (cy 2 (- hgt 1))
	(let ((g (copy-grid gold)))
	  ;; copy grid ?
	  ;; place cheat at cx cy - an empty square
	  (g 'set! cx cy #\.)

	  
	  (when (zero? (modulo step 100)) (format #t "progress ... wander ~a of ~a ~%" step total-steps))
	  (incf! step)
	  
	  ;; wander
	  (let* ((exit (wander! g))
		 (save (- nominal exit))
		 (pos (list cx cy)))
	    ;; only save if we actually saved some time
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
    (format #t "THE SOLUTION LOOKING FOR SHOULD BE ~a SOLUTIONS in total saving more than or equal to 100 picosecnds~%" tot)    
    tot))


	  

(define (part-1)
  (let ((g (populate grid-list)))
    (cheat g)))


;; %%%%%%%%%%%%%%%%%% single core takes 3 seconds %%%%%%%%%%%%%%%%%%%
;; #;1825> ,t (part-1)
;; there are 1 solutions saving 64 picoseconds
;; there are 3 solutions saving 12 picoseconds
;; there are 2 solutions saving 10 picoseconds
;; there are 4 solutions saving 8 picoseconds
;; there are 2 solutions saving 6 picoseconds
;; there are 14 solutions saving 4 picoseconds
;; there are 14 solutions saving 2 picoseconds
;; there are 1 solutions saving 20 picoseconds
;; there are 1 solutions saving 40 picoseconds
;; there are 1 solutions saving 38 picoseconds
;; there are 1 solutions saving 36 picoseconds
;; 3.114s CPU time, 0.212s GC time (major), 2654156/212942 mutations (total/tracked), 33/81764 GCs (major/minor), maximum live heap: 10.91 MiB
;; 44




;; run this 
(part-1)

;; ;; %%%%%%%%%%% the cheat %%%%%%%%%%%%%%%%%%%%
;; ;; given grid g3 -> make g g2 copies , can mutate g g2 without worry g3 corrupt
;; (define (cheat g3)
;;   (let ((g (copy-grid g3))
;; 	(start (g 'start))
;; 	(end (g 'end))
;; 	(width (g 'wid))
;; 	(height (g 'hgt))
;; 	(cheat-hash (make-hash-table))
;; 	(nominal (let ((g2 (copy-grid g3))) ;; get a nominal value to compare cheats with
;; 		   (let ((exit (wander! g2)))
;; 		     (show-grid g2)
;; 		     exit))))

;;     ;; try put an integer at x y where there is an empty #\. char
    
;;     ;; is the cheat square on the board , no point putting a cheat square on border
;;     ;; because it does not go anywhere 
;;     (define (go2 x y) 
;;       (cond
;;        ((<= x 1) #f)
;;        ((<= y 1) #f)       
;;        ((>= x width) #f)
;;        ((>= y height) #f)
;;        (#t (let ((copy (copy-hash h)))
;; 	     (g 'set! x y #\.)
;; 	     (hset! copy 'cheat-1 (list x y))
;; 	     (let* ((exit (wander! copy))
;; 		    (savings (- nominal exit)))
;; 	       (when (> savings 0) 
;; 		 (fmt #t "placing cheat at (~a ~a) saved ~a picoseconds~%" x y savings)
;; 		 (let ((val (href/d cheat-hash savings #f)))
;; 		   (cond
;; 		    ((and (list? val) (member (list x y) val)) #f)
;; 		    ((list? val) (hset! cheat-hash savings (cons (list x y) val)))
;; 		    ((eq? val #f) (hset! cheat-hash savings (list (list x y))))))
;; 		 ;;(show-hash copy)
;; 		 ;;(fmt #t "~%~%")
;; 		 ))))))
    
       
;;     ;; for cx from 2 to (- wid 1)
;;     ;; for cy from 2 to (- hgt 1)
;;     ;;   place cheat at cx cy
;;     ;;   wander 
;;     ;; loop 
;;     (let loop ((x 1)(y 1))
;; 	(cond
;; 	 ((> x width)	  
;; 	  (loop 1 (+ y 1)))
;; 	 ((> y height) #f)
;; 	 (#t

;; 	  ;; (go x y (- x 1) y)
;; 	  ;; (go x y (+ x 1) y)
;; 	  ;; (go x y x (- y 1))
;; 	  ;; (go x y x (+ y 1))

;; 	  (go2 (- x 1) y)
;; 	  (go2 (+ x 1) y)
;; 	  (go2 x (- y 1))
;; 	  (go2 x (+ y 1))
;; 	  (loop (+ x 1) y))))
    
;;     ;; go through the cheat hash what do i find
;;     (let ((keys (hash-table-keys cheat-hash))
;; 	  (tot 0))
;;       (do-list (pico keys)
;; 	       (let* ((val (href cheat-hash pico))
;; 		      (ncheats (length val)))
;; 		 (fmt #t "There are ~a cheats that save ~a picoseconds~%" ncheats pico)
;; 		 ;; how many cheats save 100 pico seconds
;; 		 (when (>= pico 100)
;; 		   (set! tot (+ tot ncheats)))))
      
;;       (fmt #t "There are ~a cheats that save 100 picoseconds~%" tot))))


#|
there are 1 solutions saving 9300 picoseconds
there are 1 solutions saving 9312 picoseconds
there are 1 solutions saving 9332 picoseconds
there are 1 solutions saving 9330 picoseconds
there are 1 solutions saving 9328 picoseconds
there are 1 solutions saving 9412 picoseconds
there are 1 solutions saving 9410 picoseconds
there are 1 solutions saving 9408 picoseconds
there are 1 solutions saving 582 picoseconds
there are 3 solutions saving 580 picoseconds
there are 2 solutions saving 578 picoseconds
there are 4 solutions saving 576 picoseconds
there are 1 solutions saving 590 picoseconds
there are 1 solutions saving 588 picoseconds
there are 2 solutions saving 584 picoseconds
there are 2 solutions saving 596 picoseconds
there are 2 solutions saving 592 picoseconds
there are 1 solutions saving 606 picoseconds
there are 2 solutions saving 604 picoseconds
there are 2 solutions saving 602 picoseconds
there are 2 solutions saving 600 picoseconds
there are 1 solutions saving 614 picoseconds
there are 2 solutions saving 612 picoseconds
there are 2 solutions saving 610 picoseconds
there are 2 solutions saving 608 picoseconds
there are 1 solutions saving 620 picoseconds
there are 1 solutions saving 618 picoseconds
there are 1 solutions saving 9380 picoseconds
there are 2 solutions saving 616 picoseconds
there are 2 solutions saving 628 picoseconds
there are 1 solutions saving 9388 picoseconds
there are 2 solutions saving 624 picoseconds
there are 1 solutions saving 9386 picoseconds
there are 1 solutions saving 638 picoseconds
there are 1 solutions saving 9384 picoseconds
there are 2 solutions saving 636 picoseconds
there are 1 solutions saving 9398 picoseconds
there are 1 solutions saving 9396 picoseconds
there are 1 solutions saving 632 picoseconds
there are 1 solutions saving 9392 picoseconds
there are 2 solutions saving 516 picoseconds
there are 3 solutions saving 512 picoseconds
there are 1 solutions saving 526 picoseconds
there are 1 solutions saving 9400 picoseconds
there are 3 solutions saving 524 picoseconds
there are 3 solutions saving 522 picoseconds
there are 3 solutions saving 520 picoseconds
there are 1 solutions saving 534 picoseconds
there are 4 solutions saving 532 picoseconds
there are 3 solutions saving 530 picoseconds
there are 3 solutions saving 528 picoseconds
there are 1 solutions saving 542 picoseconds
there are 1 solutions saving 540 picoseconds
there are 1 solutions saving 536 picoseconds
there are 1 solutions saving 548 picoseconds
there are 1 solutions saving 544 picoseconds
there are 2 solutions saving 558 picoseconds
there are 3 solutions saving 556 picoseconds
there are 2 solutions saving 554 picoseconds
there are 2 solutions saving 552 picoseconds
there are 2 solutions saving 566 picoseconds
there are 3 solutions saving 564 picoseconds
there are 1 solutions saving 562 picoseconds
there are 2 solutions saving 560 picoseconds
there are 1 solutions saving 574 picoseconds
there are 4 solutions saving 572 picoseconds
there are 3 solutions saving 570 picoseconds
there are 4 solutions saving 568 picoseconds
there are 1 solutions saving 710 picoseconds
there are 1 solutions saving 708 picoseconds
there are 1 solutions saving 704 picoseconds
there are 1 solutions saving 718 picoseconds
there are 1 solutions saving 716 picoseconds
there are 1 solutions saving 712 picoseconds
there are 1 solutions saving 726 picoseconds
there are 1 solutions saving 724 picoseconds
there are 1 solutions saving 722 picoseconds
there are 2 solutions saving 720 picoseconds
there are 2 solutions saving 732 picoseconds
there are 1 solutions saving 730 picoseconds
there are 2 solutions saving 728 picoseconds
there are 1 solutions saving 740 picoseconds
there are 2 solutions saving 748 picoseconds
there are 1 solutions saving 746 picoseconds
there are 1 solutions saving 744 picoseconds
there are 1 solutions saving 756 picoseconds
there are 1 solutions saving 754 picoseconds
there are 1 solutions saving 752 picoseconds
there are 1 solutions saving 640 picoseconds
there are 1 solutions saving 660 picoseconds
there are 1 solutions saving 658 picoseconds
there are 1 solutions saving 656 picoseconds
there are 1 solutions saving 676 picoseconds
there are 1 solutions saving 674 picoseconds
there are 1 solutions saving 672 picoseconds
there are 2 solutions saving 692 picoseconds
there are 1 solutions saving 690 picoseconds
there are 1 solutions saving 688 picoseconds
there are 2 solutions saving 700 picoseconds
there are 2 solutions saving 698 picoseconds
there are 2 solutions saving 696 picoseconds
there are 1 solutions saving 844 picoseconds
there are 1 solutions saving 842 picoseconds
there are 1 solutions saving 840 picoseconds
there are 1 solutions saving 852 picoseconds
there are 1 solutions saving 862 picoseconds
there are 2 solutions saving 860 picoseconds
there are 1 solutions saving 858 picoseconds
there are 1 solutions saving 856 picoseconds
there are 1 solutions saving 864 picoseconds
there are 1 solutions saving 806 picoseconds
there are 1 solutions saving 804 picoseconds
there are 1 solutions saving 808 picoseconds
there are 1 solutions saving 1000 picoseconds
there are 1 solutions saving 2636 picoseconds
there are 1 solutions saving 2634 picoseconds
there are 1 solutions saving 2632 picoseconds
there are 1 solutions saving 2644 picoseconds
there are 1 solutions saving 916 picoseconds
there are 1 solutions saving 914 picoseconds
there are 1 solutions saving 912 picoseconds
there are 1 solutions saving 926 picoseconds
there are 1 solutions saving 924 picoseconds
there are 1 solutions saving 928 picoseconds
there are 1 solutions saving 950 picoseconds
there are 1 solutions saving 948 picoseconds
there are 1 solutions saving 946 picoseconds
there are 1 solutions saving 944 picoseconds
there are 1 solutions saving 952 picoseconds
there are 10 solutions saving 102 picoseconds
there are 23 solutions saving 100 picoseconds
there are 9 solutions saving 110 picoseconds
there are 24 solutions saving 108 picoseconds
there are 12 solutions saving 106 picoseconds
there are 22 solutions saving 104 picoseconds
there are 8 solutions saving 118 picoseconds
there are 23 solutions saving 116 picoseconds
there are 9 solutions saving 114 picoseconds
there are 19 solutions saving 112 picoseconds
there are 9 solutions saving 126 picoseconds
there are 15 solutions saving 124 picoseconds
there are 8 solutions saving 122 picoseconds
there are 17 solutions saving 120 picoseconds
there are 3 solutions saving 198 picoseconds
there are 7 solutions saving 196 picoseconds
there are 4 solutions saving 194 picoseconds
there are 11 solutions saving 192 picoseconds
there are 4 solutions saving 206 picoseconds
there are 10 solutions saving 204 picoseconds
there are 5 solutions saving 202 picoseconds
there are 11 solutions saving 200 picoseconds
there are 2 solutions saving 214 picoseconds
there are 8 solutions saving 212 picoseconds
there are 4 solutions saving 210 picoseconds
there are 10 solutions saving 208 picoseconds
there are 3 solutions saving 222 picoseconds
there are 6 solutions saving 220 picoseconds
there are 1 solutions saving 218 picoseconds
there are 4 solutions saving 216 picoseconds
there are 3 solutions saving 230 picoseconds
there are 8 solutions saving 228 picoseconds
there are 4 solutions saving 226 picoseconds
there are 9 solutions saving 224 picoseconds
there are 2 solutions saving 238 picoseconds
there are 4 solutions saving 236 picoseconds
there are 6 solutions saving 232 picoseconds
there are 2 solutions saving 246 picoseconds
there are 9 solutions saving 244 picoseconds
there are 7 solutions saving 242 picoseconds
there are 10 solutions saving 240 picoseconds
there are 7 solutions saving 254 picoseconds
there are 9 solutions saving 252 picoseconds
there are 1 solutions saving 250 picoseconds
there are 6 solutions saving 248 picoseconds
there are 10 solutions saving 134 picoseconds
there are 15 solutions saving 132 picoseconds
there are 8 solutions saving 130 picoseconds
there are 19 solutions saving 128 picoseconds
there are 9 solutions saving 142 picoseconds
there are 19 solutions saving 140 picoseconds
there are 8 solutions saving 138 picoseconds
there are 19 solutions saving 136 picoseconds
there are 5 solutions saving 150 picoseconds
there are 14 solutions saving 148 picoseconds
there are 2 solutions saving 146 picoseconds
there are 13 solutions saving 144 picoseconds
there are 4 solutions saving 158 picoseconds
there are 11 solutions saving 156 picoseconds
there are 4 solutions saving 154 picoseconds
there are 16 solutions saving 152 picoseconds
there are 6 solutions saving 166 picoseconds
there are 13 solutions saving 164 picoseconds
there are 5 solutions saving 162 picoseconds
there are 10 solutions saving 160 picoseconds
there are 6 solutions saving 174 picoseconds
there are 13 solutions saving 172 picoseconds
there are 6 solutions saving 170 picoseconds
there are 13 solutions saving 168 picoseconds
there are 6 solutions saving 182 picoseconds
there are 13 solutions saving 180 picoseconds
there are 5 solutions saving 178 picoseconds
there are 10 solutions saving 176 picoseconds
there are 6 solutions saving 190 picoseconds
there are 14 solutions saving 188 picoseconds
there are 6 solutions saving 186 picoseconds
there are 14 solutions saving 184 picoseconds
there are 1 solutions saving 326 picoseconds
there are 1 solutions saving 324 picoseconds
there are 6 solutions saving 320 picoseconds
there are 2 solutions saving 334 picoseconds
there are 2 solutions saving 332 picoseconds
there are 1 solutions saving 328 picoseconds
there are 5 solutions saving 342 picoseconds
there are 6 solutions saving 340 picoseconds
there are 1 solutions saving 7814 picoseconds
there are 1 solutions saving 338 picoseconds
there are 1 solutions saving 7812 picoseconds
there are 2 solutions saving 336 picoseconds
there are 1 solutions saving 350 picoseconds
there are 1 solutions saving 7808 picoseconds
there are 3 solutions saving 348 picoseconds
there are 2 solutions saving 346 picoseconds
there are 7 solutions saving 344 picoseconds
there are 4 solutions saving 358 picoseconds
there are 1 solutions saving 7816 picoseconds
there are 5 solutions saving 356 picoseconds
there are 2 solutions saving 354 picoseconds
there are 2 solutions saving 352 picoseconds
there are 3 solutions saving 366 picoseconds
there are 5 solutions saving 364 picoseconds
there are 2 solutions saving 362 picoseconds
there are 6 solutions saving 360 picoseconds
there are 3 solutions saving 374 picoseconds
there are 5 solutions saving 372 picoseconds
there are 3 solutions saving 370 picoseconds
there are 6 solutions saving 368 picoseconds
there are 1 solutions saving 382 picoseconds
there are 5 solutions saving 380 picoseconds
there are 3 solutions saving 378 picoseconds
there are 7 solutions saving 376 picoseconds
there are 1 solutions saving 262 picoseconds
there are 6 solutions saving 260 picoseconds
there are 3 solutions saving 258 picoseconds
there are 11 solutions saving 256 picoseconds
there are 1 solutions saving 270 picoseconds
there are 6 solutions saving 268 picoseconds
there are 3 solutions saving 266 picoseconds
there are 4 solutions saving 264 picoseconds
there are 1 solutions saving 278 picoseconds
there are 4 solutions saving 276 picoseconds
there are 2 solutions saving 274 picoseconds
there are 4 solutions saving 272 picoseconds
there are 3 solutions saving 286 picoseconds
there are 5 solutions saving 284 picoseconds
there are 1 solutions saving 282 picoseconds
there are 3 solutions saving 280 picoseconds
there are 1 solutions saving 294 picoseconds
there are 7 solutions saving 292 picoseconds
there are 1 solutions saving 290 picoseconds
there are 8 solutions saving 288 picoseconds
there are 1 solutions saving 302 picoseconds
there are 5 solutions saving 300 picoseconds
there are 2 solutions saving 298 picoseconds
there are 5 solutions saving 296 picoseconds
there are 2 solutions saving 310 picoseconds
there are 4 solutions saving 308 picoseconds
there are 3 solutions saving 306 picoseconds
there are 3 solutions saving 304 picoseconds
there are 4 solutions saving 318 picoseconds
there are 6 solutions saving 316 picoseconds
there are 2 solutions saving 314 picoseconds
there are 5 solutions saving 312 picoseconds
there are 2 solutions saving 454 picoseconds
there are 3 solutions saving 452 picoseconds
there are 2 solutions saving 448 picoseconds
there are 2 solutions saving 460 picoseconds
there are 1 solutions saving 458 picoseconds
there are 3 solutions saving 456 picoseconds
there are 2 solutions saving 470 picoseconds
there are 3 solutions saving 468 picoseconds
there are 1 solutions saving 466 picoseconds
there are 1 solutions saving 464 picoseconds
there are 3 solutions saving 478 picoseconds
there are 4 solutions saving 476 picoseconds
there are 1 solutions saving 474 picoseconds
there are 3 solutions saving 472 picoseconds
there are 1 solutions saving 486 picoseconds
there are 3 solutions saving 484 picoseconds
there are 2 solutions saving 482 picoseconds
there are 4 solutions saving 480 picoseconds
there are 3 solutions saving 492 picoseconds
there are 1 solutions saving 490 picoseconds
there are 3 solutions saving 488 picoseconds
there are 1 solutions saving 500 picoseconds
there are 1 solutions saving 496 picoseconds
there are 2 solutions saving 510 picoseconds
there are 2 solutions saving 508 picoseconds
there are 1 solutions saving 506 picoseconds
there are 2 solutions saving 504 picoseconds
there are 4 solutions saving 388 picoseconds
there are 3 solutions saving 386 picoseconds
there are 5 solutions saving 384 picoseconds
there are 1 solutions saving 396 picoseconds
there are 1 solutions saving 392 picoseconds
there are 2 solutions saving 404 picoseconds
there are 1 solutions saving 402 picoseconds
there are 1 solutions saving 2158 picoseconds
there are 1 solutions saving 400 picoseconds
there are 1 solutions saving 2156 picoseconds
there are 1 solutions saving 414 picoseconds
there are 3 solutions saving 412 picoseconds
there are 1 solutions saving 410 picoseconds
there are 1 solutions saving 408 picoseconds
there are 1 solutions saving 422 picoseconds
there are 1 solutions saving 420 picoseconds
there are 1 solutions saving 2160 picoseconds
there are 4 solutions saving 416 picoseconds
there are 1 solutions saving 430 picoseconds
there are 3 solutions saving 428 picoseconds
there are 1 solutions saving 424 picoseconds
there are 1 solutions saving 438 picoseconds
there are 2 solutions saving 436 picoseconds
there are 1 solutions saving 432 picoseconds
there are 1 solutions saving 446 picoseconds
there are 3 solutions saving 444 picoseconds
there are 1 solutions saving 2056 picoseconds
there are 1 solutions saving 442 picoseconds
there are 2 solutions saving 440 picoseconds
there are 1 solutions saving 9038 picoseconds
there are 1 solutions saving 9036 picoseconds
there are 1 solutions saving 9040 picoseconds
there are 1 solutions saving 2084 picoseconds
there are 1 solutions saving 2082 picoseconds
there are 1 solutions saving 2080 picoseconds
there are 1 solutions saving 9070 picoseconds
there are 1 solutions saving 9068 picoseconds
there are 1 solutions saving 2096 picoseconds
there are 1 solutions saving 9066 picoseconds
there are 1 solutions saving 9064 picoseconds
there are 1 solutions saving 9076 picoseconds
there are 1 solutions saving 9074 picoseconds
there are 1 solutions saving 9072 picoseconds
there are 1 solutions saving 2260 picoseconds
there are 1 solutions saving 7244 picoseconds
there are 1 solutions saving 7242 picoseconds
there are 1 solutions saving 2292 picoseconds
there are 1 solutions saving 7240 picoseconds
there are 1 solutions saving 1572 picoseconds
there are 1 solutions saving 2288 picoseconds
there are 1 solutions saving 1570 picoseconds
there are 1 solutions saving 7252 picoseconds
there are 1 solutions saving 1568 picoseconds
there are 1 solutions saving 2300 picoseconds
there are 1 solutions saving 2298 picoseconds
there are 1 solutions saving 2296 picoseconds
there are 1 solutions saving 1584 picoseconds
there are 1 solutions saving 1596 picoseconds
there are 1 solutions saving 2184 picoseconds
there are 1 solutions saving 1594 picoseconds
there are 1 solutions saving 1592 picoseconds
there are 1 solutions saving 9152 picoseconds
there are 1 solutions saving 2196 picoseconds
there are 1 solutions saving 2194 picoseconds
there are 1 solutions saving 2192 picoseconds
there are 1 solutions saving 9102 picoseconds
there are 1 solutions saving 9100 picoseconds
there are 1 solutions saving 9104 picoseconds
there are 1 solutions saving 9118 picoseconds
there are 1 solutions saving 9116 picoseconds
there are 1 solutions saving 9114 picoseconds
there are 1 solutions saving 9112 picoseconds
there are 1 solutions saving 9120 picoseconds
there are 1 solutions saving 9132 picoseconds
there are 1 solutions saving 7388 picoseconds
there are 1 solutions saving 7386 picoseconds
there are 1 solutions saving 9150 picoseconds
there are 1 solutions saving 7384 picoseconds
there are 2 solutions saving 9148 picoseconds
there are 1 solutions saving 9146 picoseconds
there are 1 solutions saving 9144 picoseconds
there are 1 solutions saving 7404 picoseconds
there are 1 solutions saving 7402 picoseconds
there are 1 solutions saving 7400 picoseconds
there are 1 solutions saving 7412 picoseconds
there are 1 solutions saving 7408 picoseconds
there are 1 solutions saving 7492 picoseconds
there are 1 solutions saving 7490 picoseconds
there are 1 solutions saving 7488 picoseconds
there are 1 solutions saving 2536 picoseconds
there are 1 solutions saving 2550 picoseconds
there are 1 solutions saving 2548 picoseconds
there are 1 solutions saving 2546 picoseconds
there are 1 solutions saving 2544 picoseconds
there are 1 solutions saving 2556 picoseconds
there are 1 solutions saving 2552 picoseconds
there are 1 solutions saving 7526 picoseconds
there are 1 solutions saving 7524 picoseconds
there are 1 solutions saving 7528 picoseconds
there are 1 solutions saving 2000 picoseconds
there are 1 solutions saving 2020 picoseconds
there are 1 solutions saving 2028 picoseconds
there are 1 solutions saving 8446 picoseconds
there are 1 solutions saving 8444 picoseconds
there are 1 solutions saving 7460 picoseconds
there are 1 solutions saving 7476 picoseconds
there are 1 solutions saving 7472 picoseconds
there are 1 solutions saving 8448 picoseconds
there are 1 solutions saving 1228 picoseconds
there are 1 solutions saving 1226 picoseconds
there are 1 solutions saving 1224 picoseconds
there are 1 solutions saving 1246 picoseconds
there are 1 solutions saving 1244 picoseconds
there are 1 solutions saving 1248 picoseconds
there are 1 solutions saving 4812 picoseconds
there are 1 solutions saving 4810 picoseconds
there are 1 solutions saving 4808 picoseconds
there are 1 solutions saving 1196 picoseconds
there are 1 solutions saving 4830 picoseconds
there are 1 solutions saving 4828 picoseconds
there are 1 solutions saving 1204 picoseconds
there are 1 solutions saving 1202 picoseconds
there are 1 solutions saving 4836 picoseconds
there are 1 solutions saving 1200 picoseconds
there are 1 solutions saving 4832 picoseconds
there are 1 solutions saving 1350 picoseconds
there are 1 solutions saving 1348 picoseconds
there are 1 solutions saving 1344 picoseconds
there are 2 solutions saving 1358 picoseconds
there are 2 solutions saving 1356 picoseconds
there are 1 solutions saving 1354 picoseconds
there are 2 solutions saving 1352 picoseconds
there are 2 solutions saving 1360 picoseconds
there are 1 solutions saving 1404 picoseconds
there are 1 solutions saving 4932 picoseconds
there are 1 solutions saving 4928 picoseconds
there are 1 solutions saving 1332 picoseconds
there are 1 solutions saving 1330 picoseconds
there are 1 solutions saving 1328 picoseconds
there are 1 solutions saving 1342 picoseconds
there are 1 solutions saving 1340 picoseconds
there are 1 solutions saving 4974 picoseconds
there are 1 solutions saving 4972 picoseconds
there are 1 solutions saving 4976 picoseconds
there are 1 solutions saving 4870 picoseconds
there are 1 solutions saving 4868 picoseconds
there are 1 solutions saving 4876 picoseconds
there are 1 solutions saving 4874 picoseconds
there are 1 solutions saving 4872 picoseconds
there are 1 solutions saving 1428 picoseconds
there are 1 solutions saving 1426 picoseconds
there are 1 solutions saving 1424 picoseconds
THE SOLUTION LOOKING FOR SHOULD BE 1358 SOLUTIONS in total saving more than or equal to 100 picosecnds

real	6m50.124s
user	6m47.514s
sys	0m2.580s


|#
