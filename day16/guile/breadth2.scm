#|

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
	       ((char=? ch #\S) (fmt #t "~a" #\S))
	       ((char=? ch #\E) (fmt #t "~a" #\E))	
	       ((char=? ch #\#) (fmt #t "~a" #\#))       
	       ((char=? ch #\.) (fmt #t "~a" #\.))
	       (#t (fmt #t "?~a?" ch))))
	    (loop-x (+ x 1)))))
	(loop-y (+ y 1)))))))



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


;; ============== RUNTIME ===================================
;; read in grid from a file
;; figure out start and end positions
;; erase those from the grid
;; grid only has either # meaning not accessible
;; or . meaning available to move through

(define g (to-array (read-example "../example1.txt")))
;;(define g (to-array (read-example "../input.txt")))
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
(define score (make-score-vector wid hgt))
;; 


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


;; do breadth first search 
(define search
  (lambda ()
    (letrec
	((hunt (lambda (x y dir cost)		 
		 
		 (cond
		  ((char=? (internal-xy g x y) #\#) #f) ;; wall
		  ((and (= x end-x) (= y end-y))
		   (fmt #t " found target at ~a ~a with cost of ~a ~%" x y cost)
		   (when (or (< cost best)(= best -1))
		     (set! best cost))
		   #t)
		  (#t ;; not a wall
		   ;;(fmt #t "at ~a ~a " x y)
		   (let ((s (score-get score x y dir)))      
		     ;; record reached a new location 
		     ;;(fmt #t " : score ~a " s)
		     
		     (when (< s 0)
		       (score-set! score x y dir cost))
		     ;; if the cost to reach this square is higher than that already found then discontinue search
		     (cond
		      ((< (score-get score x y dir) cost)
		       ;;(fmt #t " : aborted ~a ~%" cost)
		       #f)
)
		      (#t
		       ;; forward
		       ;;(fmt #t "forward ! ~%")

		       (cond
			((eq? dir 'east) (hunt (+ x 1) y dir (+ cost 1)))
			((eq? dir 'west) (hunt (- x 1) y dir (+ cost 1)))
			((eq? dir 'south) (hunt x (+ y 1) dir (+ cost 1)))
			((eq? dir 'north) (hunt x (- y 1) dir (+ cost 1)))
			(#t (error "hunt bad dir")))
		       
		       ;; turn left
		       ;;(fmt #t "left ! ~%")
		       (hunt x y (clockwise dir) (+ cost 1000))
		       
		       ;; turn right
		       ;;(fmt #t "right ! ~%")
		       (hunt x y (anticlockwise dir) (+ cost 1000))
		       )))))))
      
      (hunt start-x start-y 'east 0))))




;; (define example1 (read-example "../example1.txt"))
;; (define example2 (read-example "../example2.txt"))
;; (define input1 (read-example "../input.txt"))


;; ;; the grid
;; (define problem #f)
;; (define g #f)
;; ;; robot position 
;; (define r #f)
;; ;; (fmt #t "robot is at ~a ~%" r)
;; (define rx #f)
;; (define ry #f)
;; (define wid #f)
;; (define hgt #f)
;; (define wid-1 #f)
;; (define hgt-1 #f) 
;; (define moves #f)




;; ;; g grid
;; ;; m move we have in mind left right up down
;; ;; we build up a stack of procedures which if eventually find we can complete all the moves
;; ;; all get executed , such that last robot moves first .. first robot moves last .. nice outcome
;; ;; albeit side effect full mutation 
;; (define (step-left)
;;   (step-generic (lambda (x) (- x 1)) (lambda (y) y)))

;; (define (step-right)
;;   (step-generic (lambda (x) (+ x 1)) (lambda (y) y)))

;; (define (step-down)
;;   (step-generic (lambda (x) x) (lambda (y) (+ y 1))))

;; (define (step-up)
;;   (step-generic (lambda (x) x) (lambda (y) (- y 1))))


;; (define (step-generic fx fy)
;;   (let* ((stack '()))
;;     (letrec ((loop (lambda (x y)
;; 		     (cond
;; 		      ;; cannot move a wall - unwind without executing stack		       
;; 		      ((and (<= x 0)(>= x wid-1) (<= y 0) (>= y hgt-1)) #f)
		       
;; 		      (#t 
;; 		       (let ((ch (internal-xy g x y)))
;; 			 ;;(fmt #t "step: looking at position ~a ~a  found char ~a ~%" x y ch)
;; 			 (cond
			  
;; 			   ;; cannot move a wall - unwind without executing stack
;; 			  ((char=? ch #\#) #f)
			  
;; 			   ;; can move object O 
;; 			  ((char=? ch #\O)
;; 			   (set! stack (cons (lambda ()
;; 					       (internal-xy! g x y #\.) ;; clear space occupied by object
;; 					       (internal-xy! g (fx x) (fy y) #\O)) ;; object is now one position left
;; 					     stack))
;; 			   ;; look at object to left
;; 			   (loop (fx x) (fy y)))
			  
;; 			  ;; empty space - activate the stack unwind			  
;; 			  ((char=? ch #\.) (execute-stack stack) #t)
;; 			  (#t
;; 			   (fmt #t "unrecognised char ~a in grid at ~a ~a ~%" ch x y)
;; 			   (error "step"))))))))
;; 	     (execute-stack (lambda (xs)
;; 			      (cond
;; 			       ((null? xs) xs)
;; 			       (#t (let ((fn (car xs)))
;; 				     (fn)
;; 				     (execute-stack (cdr xs))))))))
;;       ;; debug
;;       ;;(fmt #t "step : robot is at ~a ~a ~%" rx ry)
;;       ;; robot will always be able to move left
;;       ;; add that move to the stack
;;       (set! stack (cons (lambda ()
;; 			  (internal-xy! g rx ry #\.) ;; clear space occupied by robot
;; 			  (internal-xy! g (fx rx) (fy ry) #\@) ;; robot is now one position left
;; 			  ;;(fmt #t "changing robot position from ~a ~a to " rx ry)
;; 			  (set! rx (fx rx)) ;; change global position of robot
;; 			  (set! ry (fy ry))
;; 			  ;;(fmt #t " =>  ~a ~a ~%" rx ry)

;; 			)
;; 			stack))
;;       (loop (fx rx) (fy ry)))))


;; (define (score-grid)
;;   (let ((score 0))
;;     (letrec ((foo (lambda (x y)
;; 		    (cond
;; 		     ((>= y hgt) score)
;; 		     ((>= x wid) (foo 0 (+ y 1)))
;; 		     (#t (let ((ch (internal-xy g x y)))
;; 			   (cond
;; 			    ((char=? ch #\O)
;; 			     (set! score (+ score (* 100 y) x))))
;; 			   (foo (+ x 1) y)))))))
;;       (foo 0 0))))








;; (define (reset)
;;   (set! example1 (read-example "../example1.txt"))
;;   (set! example2 (read-example "../example2.txt"))
;;   (set! input1 (read-example "../input.txt"))
;;   ;; choose your weapon 
;;   (set! problem input1)
;;   (set! g (to-array (find-grid problem)))
;;   (set! r (find-robot g))
;;   ;; (fmt #t "robot is at ~a ~%" r)
;;   (set! rx (car r))
;;   (set! ry (car (cdr r)))
;;   (set! wid (second (assoc 'wid g)))
;;   (set! hgt (second (assoc 'hgt g)))
;;   (set! wid-1 (- wid 1))
;;   (set! hgt-1 (- hgt 1))
;; ;;  (set! moves (find-moves problem))
;; )  


;; (define (run)
;;   (reset)
;;   (fmt #t "running ........~%")
;;   ;; know where robot is at all times using rx ry
;;   (let ((count -1))
;;     (letrec ((next (lambda (xs)
;; 		     (set! count (+ count 1))
;; 		     ;;(fmt #t "count [~a] ~%" count)
;; 		     ;;(show-grid g)
;; 		     (cond
;; 		      ((null? xs) #t)
;; 		      (#t (let ((ch (car xs)))			    
;; 			    ;;(fmt #t " : move [~a] ~%" ch)			    
;; 			    (cond
;; 			     ((char=? ch #\<) (step-left))
;; 			     ((char=? ch #\>) (step-right))
;; 			     ((char=? ch #\^) (step-up))
;; 			     ((char=? ch #\v) (step-down))
;; 			     (#t (fmt #t "run next ch ~a : error bad char~%" ch)
;; 				 (error "next")))
;; 			    (next (cdr xs))))))))
;;       (next moves)
;;       (let ((result (score-grid)))
;; 	(fmt #t "score = ~a ~%" result)
;; 	result))))





	       


;; #|
;; think array may not be best approach here

;; (define g (to-array (find-grid example2)))
;; (internal-xy g 2 2)
;; (internal-xy g 2 4)
;; (internal-xy g 3 1)
;; ;;(internal-xy! g 3 1 #\t)
;; (pp g )
;; ;;(internal-xy! g 0 0 #\s)
;; (pp g )
;; (define gm (find-moves example2))

;; #\# is a wall character
;; #\. is empty square
;; #\O is something to move like a box
;; #\@ is the robot

;; we always start with robot x , y 
;; look at move  direction < left   right >   ^up  down v
;; depednign on the direction we look to see what implications of making the move

;; robot to go left
;; robot next position is (x-1,y)
;; look at (x-1,y) and ask is there push back - ie refusal to move ?
;; if #\# wall character then there is pushback , simply move onto next move , nobody moves
;; if #\. empty no pushback , everything mentioned up till now gets to move
;; if O object , then it will also ask neighbour in direction of move if that too can move ?
;; builds up a stack , if at end we find we can make the moves , then the stack is unwound with moves applied
;; and when stack is empty , next move can be applied


;; |#













