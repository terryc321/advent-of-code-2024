
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
	   ((eq? op 'attr) (attr args)) ;; attributes 
	   ((eq? op 'attr!) (attr! args))
	   
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
			    (g 'set! x y n))
			   ((and (integer? ch) (< n ch)) ;; an integer here
			    ;; unlikely this ever gets executed because all values updated in one fell swoop
			    (fmt #t "found better value for square ~a ~a" x y)
			    (set! did-set #t)
			    (g 'set! x y n))))))))
	     ;; otherwise i dont care #\# wall or border of grid
	     (forward (lambda (n)
			(for (x 1 wid)
			  (for (y 1 hgt)
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
(define g2 (copy-grid g))

(g 'show)

(g 'get 2 4)
(g 'start)
(g 'end)
(wander! g)
(g 'show)

(g2 'show)


(define (cheat gold)
  (let ((start (g 'start))
	(end (g 'end))
	(wid (g 'wid))
	(hgt (g 'hgt))
	(nominal (wander! (copy-grid gold)))
	(hash (make-hash-table)))
    (for (cx 2 (- wid 1))
      (for (cy 2 (- hgt 1))
	(let ((g (copy-grid gold)))
	  ;; copy grid ?
	  ;; place cheat at cx cy - an empty square
	  (g 'set! cx cy #\.)
	  ;; wander
	  (let* ((exit (wander! g))
		 (save (- nominal exit))
		 (pos (list cx cy)))
	    ;; only save if we actually saved some time
	    (when (> save 0)
	    ;; store location cx cy under savings key
	    (let ((hv (href/d hash save #f)))
	      (cond
	       ((eq? hv #f) (hset! hash save (list pos)))
	       ((member pos hv) #f)
	       (#t (hset! hash save (cons pos hv))))))))))
    
    ;; for each - list them out in some order?
    (do-list (save (hkeys hash))
	     (let ((coords (href hash save)))
	       (format #t "there are ~a solutions saving ~a picoseconds~%" (length coords) save)))))


	  

(define (part-1)
  (let ((g (populate grid-list)))
    (cheat g)))


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



