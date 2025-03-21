#|

this is a test to see if i can make a change to breadth2 file on old computer


* weird 2d implementation of grid using association lists ??? dont ask *

start at S location facing east
need to reach square E however we can

working group = collection of squares to be processed

todo group = squares that have been generated from the working group

when the working group is exhausted , the todo group becomes the working group and the todo group
becomes empty

start at S facing east

working group = [ S ]
todo group = []

we can go forward incur a cost of +1
we can turn left incur cost of +1000
we can turn right incur cost of +1000

working group = []
todo group = [ s_forward_facing_east_at_cost_1
               s_left_facing_north_at_cost_1000
               s_right_facing_south_at_cost_1000 ]

for each state P , we can go forward , turn left , turn right , each state remembers its current cost

grid will remember the best cost for every square given a specific direction 

know definitively if return to a given square with a higher cost facing same direction as been before then 
no need to continue processing this state and it is simply not recorded further

the cost of every square can be #f as never set

as soon as arrive at square, generate all four directions and their costs right right right right , left left left left
that will then set a limit for any future visits to that square where we can immediately tell if
such a state is sub optimal

grid of width x height
each element of grid will be array of four values representing facing north , east ,south ,west
these values are the true cost to reach this location

if we arrive at a square and find we have a lower cost , we record that in the square , again transform right right right etc,
transform left left left 
if state is equal to known score , then do not add it to the to do list
if state is less than current score , add it to the todo list and record in the grid

the todo list will then have a set of squares that have potential to be optimal

|#

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

(define wid #f)
(define hgt #f)



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


;;(to-array (find-grid example2))
(define to-array
  (lambda (xs)
    (let* ((hgt2 (length xs))
	   (wid2 (length (car xs)))
	   (grid (make-vector hgt2 0))
	   (rows xs)
	   (cols #f))
      (set! wid wid2)
      (set! hgt hgt2)
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

;; how define it ? how does show-grid do it ?
(define (in-grid? g x y)
  (let ((wid (second (assoc 'wid g)))
	(hgt (second (assoc 'hgt g))))
    (and (>= x 0)(>= y 0)(< x wid)(< y hgt))))

;; at-grid :: total
;; either returns false meaning you are off the grid itself
;; OR returns what should be at the grid
;; doesnt type check but could massage it to a type
(define (at-grid g x y)
  (let ((wid (second (assoc 'wid g)))
	(hgt (second (assoc 'hgt g))))
    (if (and (>= x 0)(>= y 0)(< x wid)(< y hgt))
	(internal-xy g x y)
	#f)))
   
(define (width-of g)
  (second (assoc 'wid g)))

(define (height-of g)
  (second (assoc 'hgt g)))

;; zero based indexes ??

  

;; given a grid , find the robot ... loops past even tho found it
;; not important

(define (find-in-grid g fn)
  (let ((wid (second (assoc 'wid g)))
	(hgt (second (assoc 'hgt g))))
    (let ((location #f))
      (catch 'done
	(lambda ()
      (let loop-y ((y 0))
	(cond
	 ((< y hgt)
	  (let loop-x ((x 0))
	    (cond
	     ((< x wid)
	      ;;(fmt #t "checking location ~a ~a ~%" x y)
	      (let ((ch (internal-xy g x y)))
		(cond
		 ((fn ch) ;; predicate 
		  (set! location (list x y))
		  (throw 'done ))))
	      (loop-x (+ x 1)))))
	  (loop-y (+ y 1))))))
	(lambda (e)
	  ;;(fmt #t "caught exception ~a ~%" e)
	  location)))))


;; ;; shows grid g 
(define (show-grid g)
  (let ((wid (second (assoc 'wid g)))
	(hgt (second (assoc 'hgt g))))
    (fmt #t "~%")
    (let loop-y ((y 0))
      (fmt #t "~%")
      (cond
       ((< y hgt)
	(let loop-x ((x 0))
	  (cond
	   ((< x wid)
	    ;;(fmt #t "checking location ~a ~a ~%" x y)
	    (let ((ch (internal-xy g x y)))
	      (cond
	       ((not (char? ch)) (fmt #t "(~a)" ch))
	       ((char=? ch #\S) (fmt #t "~a" #\S))
	       ((char=? ch #\E) (fmt #t "~a" #\E))	
	       ((char=? ch #\#) (fmt #t "~a" #\#))       
	       ((char=? ch #\.) (fmt #t "~a" #\.))
	       (#t (fmt #t "[~a]" ch))))
	    (loop-x (+ x 1)))))
	(loop-y (+ y 1)))))))


;; ===================================================================================


(define make-four-vector
  (lambda () (make-vector 4 -1)))

(define make-wid-vector
  (lambda (wid)
    (let ((res (make-vector wid 0)))
      ;; loop over with x 
      (let loop ((x 0))
	(vector-set! res x (make-four-vector))
	(when (< x (- wid 1))
	  (loop (+ x 1))))
      res)))

;; generates a 2d grid of wid x hgt with each having vector.length(four) -1 -1 -1 -1
(define make-score-vector
  (lambda (wid hgt)
    (let ((res (make-vector hgt 0)))
      ;; loop over with y 
      (let loop ((y 0))
	(vector-set! res y (make-wid-vector wid))
	(when (< y (- hgt 1))
	  (loop (+ y 1))))
      res)))


;; takes position and direction and sets the score index 
(define score-set!
  (lambda (score x y dir s)
    (let ((di     (cond
		   ((eq? dir 'north) 0)
		   ((eq? dir 'east) 1)
		   ((eq? dir 'west) 2)
		   ((eq? dir 'south) 3)
		   (#t (error "score-set! bad dir")))))
      (let* ((yvec (vector-ref score y))
	     (xvec (vector-ref yvec x)))
	(vector-set! xvec di s)))))


(define score-get
  (lambda (score x y dir)
    (let ((di     (cond
		   ((eq? dir 'north) 0)
		   ((eq? dir 'east) 1)
		   ((eq? dir 'west) 2)
		   ((eq? dir 'south) 3)
		   (#t (error "score-set! bad dir")))))
      (let* ((yvec (vector-ref score y))
	     (xvec (vector-ref yvec x)))
	(vector-ref xvec di)))))


;; ======================================================



;; set initial direction east at start square as a score of 0 	     
;; (let* ((xy start)(x (car xy))(y (car(cdr xy))))
;;   (score-set! x y 'east 0))

;; score grid where each element of grid has its own array size 4
;; to represent the minimum score required to reach that square whilst facing a certain direction
(define east 'east)
(define west 'west)
(define north 'north)
(define south 'south)
(define cost 0)

(define (anticlockwise dir)
  (cond
   ((eq? dir 'north) 'west)
   ((eq? dir 'east) 'north)
   ((eq? dir 'west) 'south)
   ((eq? dir 'south) 'east)
   (#t (error "anticlockwise bad dir"))))


(define (clockwise dir)
  (cond
   ((eq? dir 'north) 'east)
   ((eq? dir 'east) 'south)
   ((eq? dir 'west) 'north)
   ((eq? dir 'south) 'west)
   (#t (error "clockwise bad dir"))))

(define best -1)   


;; ============== RUNTIME ===================================
;; read in grid from a file
;; figure out start and end positions
;; erase those from the grid
;; grid only has either # meaning not accessible
;; or . meaning available to move through

;;(define g (to-array (read-example "../example1.txt")))
;;(define g (to-array (read-example "../example2.txt")))
(define g (to-array (read-example "../input.txt")))

(define start (find-in-grid g (lambda (ch) (char=? ch #\S))))
(define end (find-in-grid g (lambda (ch) (char=? ch #\E))))

(define start-x (car start))
(define start-y (car (cdr start)))

(define end-x (car end))
(define end-y (car (cdr end)))

;; erase S in grid 
;; erase E in grid
(let* ((xy start)(x (car xy))(y (car(cdr xy))))
  (internal-xy! g x y #\.))
(let* ((xy end)(x (car xy))(y (car(cdr xy))))
  (internal-xy! g x y #\.))

(set! wid (second (assoc 'wid g)))
(set! hgt (second (assoc 'hgt g)))

;; score doesnt need to be reset
;;(define score (make-score-vector wid hgt))
;;

;; (anticlockwise 'east)
;; (anticlockwise 'north)
;; (anticlockwise 'west)
;; (anticlockwise 'south)

;; state s = x y dir cost

#|
turning left or right simply updates new direction and adds 1000 onto current cost
|#
(define (state-left s)
  (let ((x (list-ref s 0))
	(y (list-ref s 1))
	(dir (list-ref s 2))
	(cost (list-ref s 3)))	
    (list x y (anticlockwise dir) (+ cost 1000))))



(define (state-right s)
  (let ((x (list-ref s 0))
	(y (list-ref s 1))
	(dir (list-ref s 2))
	(cost (list-ref s 3)))
    (list x y (clockwise dir) (+ cost 1000))))



#|
going forward simply increase cost by 1 in direction dir suggests
we make no attempt to determine from a grand high level overview if such a state change
is in any way legal or illegal
|#
(define (state-forward s)
  (let ((x (list-ref s 0))
	(y (list-ref s 1))
	(dir (list-ref s 2))
	(cost (list-ref s 3)))
    (cond
     ((eq? dir 'north) (list x (- y 1) dir (+ cost 1)))
     ((eq? dir 'east)  (list (+ x 1) y dir (+ cost 1)))
     ((eq? dir 'south) (list x (+ y 1) dir (+ cost 1)))
     ((eq? dir 'west)  (list (- x 1) y dir (+ cost 1)))
     (#t (error (format #f "bad direction state foward on ~a " s))))))



#|
determinining if a state is legal
 depends on the grid we are working with 
 is it inside the boundary of grid
 is what is at the grid x y an empty square '.'
|#
(define (legal? g s)
  (let ((x (car s))
	(y (cadr s))
	(dir (caddr s))
	(cost (cadddr s)))
    (equal? (at-grid g x y) #\.)))

;; worthy of further consideration
;; hash table with (x y dir) as key
;; value is the cost
;; if no entry in hash table then yes should be considered further as lead to new routes
;; if cost value in hash is lower than cost value in state - do not consider further
;; if cost value in hash is equal to cost value in state - do not consider further
;; if cost value in hash is greater than cost value in state - found a better route
;;  definitely consider further


;; FURTHER : global hash table takes in state and make recommendation to keep state
;; LOOKUP :  given x y coordinates shows the minimal states that reached here
(define further? #f)
(define lookup #f)
(let ((h (make-hash-table)))
  (letrec ((foo (lambda (s)		  
		  (let ((x (car s))
			(y (cadr s))
			(dir (caddr s))
			(cost (cadddr s)))
		    (let* ((key (list x y dir))
			   (value (hash-ref h key 'no-entry)))
		      (cond
		       ((eq? value 'no-entry) (hash-set! h key cost) #t)
		       ((<= value cost) #f)
		       (#t (hash-set! h key cost) #t))))))
	   (bar (lambda (x y)
		  (list (hash-ref h (list x y 'north) 'no-entry)
			(hash-ref h (list x y 'east) 'no-entry)
			(hash-ref h (list x y 'south) 'no-entry)
			(hash-ref h (list x y 'west) 'no-entry)))))
    (set! further? foo)
    (set! lookup bar)))



(define zero-cost 0)
(define empty-path '())
(define work-set (list (list start-x start-y 'east zero-cost)))
(define todo-set '())

(define (find)
  (cond
   ((null? work-set)
    (set! work-set todo-set)
    (set! todo-set '())
    (cond
     ((null? work-set) 'done-do-lookup-on-end-x-end-y)
     (#t (find))))
   (#t (let ((s (car work-set)))
	 (set! work-set (cdr work-set))
	 (let ((sleft (state-left s))
	       (sright (state-right s))
	       (sforward (state-forward s)))
	   (letrec ((consider! (lambda (x)
				 (when (and (legal? g x) (further? x))
				   (set! todo-set (cons x todo-set))))))
	     (consider! s)
	     (consider! sleft)
	     (consider! sright)
	     (consider! sforward)
	     (find)))))))


(define (part-1)
  (find)
  (sort (lookup end-x end-y) <))

#|
scheme@(guile-user)> (part-1)
$90 = (133584 134584 134584 135584)
        ^^^--- best score

133584 accepted answer

|#

