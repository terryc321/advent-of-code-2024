#|

interesting unwinding pattern using a stack and procedures that are called on success
moves a block of things forward one space on each item in the train
front train moves first - also checked last if it can move forward
back of train moves last - checked first if it can move - which in turn causes next carriage to be checked if
it can move forward


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


;; keep taking until nil  
(define find-grid
  (lambda (xs)
    (cond
     ((null? xs) xs)
     (#t (let ((elem (car xs)))
	   (cond
	    ((null? elem) '())
	    (#t (cons elem (find-grid (cdr xs))))))))))

;; traverse until find null element , moves are rest of those
;; because we slurp lines and moves are long and cover many lines
;; may need to slurp moves into one list
;; example2 only 15 moves , so no line wrap and 1st element of find-moves2 is not a list
;; example1 has 10 wrapped lines and means 10 lists each containing 70 elements , so
;; apply append lines - makes all contents of all lines come together into a single stream
(define find-moves
  (lambda (xs)
    (letrec ((find-moves2  (lambda (xs)
			     (cond
			      ((null? xs) xs)
			      (#t (let ((elem (car xs)))
				    (cond
				     ((null? elem) (cdr xs))
				     (#t (find-moves2 (cdr xs))))))))))
      (let ((result (find-moves2 xs)))
	(cond
	 ((list? (car result)) (apply append result))
	 (#t result))))))




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

;; given a grid , find the robot ... loops past even tho found it 
(define (find-robot g)
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
		 ((char=? ch #\@) (set! location (list x y))
		  (throw 'done ))))
	      (loop-x (+ x 1)))))
	  (loop-y (+ y 1))))))
	(lambda (e)
	  ;;(fmt #t "caught exception ~a ~%" e)
	  location)))))

(define example1 (read-example "../example1.txt"))
(define example2 (read-example "../example2.txt"))
(define input1 (read-example "../input.txt"))


;; the grid
(define problem #f)
(define g #f)
;; robot position 
(define r #f)
;; (fmt #t "robot is at ~a ~%" r)
(define rx #f)
(define ry #f)
(define wid #f)
(define hgt #f)
(define wid-1 #f)
(define hgt-1 #f) 
(define moves #f)




;; g grid
;; m move we have in mind left right up down
;; we build up a stack of procedures which if eventually find we can complete all the moves
;; all get executed , such that last robot moves first .. first robot moves last .. nice outcome
;; albeit side effect full mutation 
(define (step-left)
  (step-generic (lambda (x) (- x 1)) (lambda (y) y)))

(define (step-right)
  (step-generic (lambda (x) (+ x 1)) (lambda (y) y)))

(define (step-down)
  (step-generic (lambda (x) x) (lambda (y) (+ y 1))))

(define (step-up)
  (step-generic (lambda (x) x) (lambda (y) (- y 1))))


(define (step-generic fx fy)
  (let* ((stack '()))
    (letrec ((loop (lambda (x y)
		     (cond
		      ;; cannot move a wall - unwind without executing stack		       
		      ((and (<= x 0)(>= x wid-1) (<= y 0) (>= y hgt-1)) #f)
		       
		      (#t 
		       (let ((ch (internal-xy g x y)))
			 ;;(fmt #t "step: looking at position ~a ~a  found char ~a ~%" x y ch)
			 (cond
			  
			   ;; cannot move a wall - unwind without executing stack
			  ((char=? ch #\#) #f)
			  
			   ;; can move object O 
			  ((char=? ch #\O)
			   (set! stack (cons (lambda ()
					       (internal-xy! g x y #\.) ;; clear space occupied by object
					       (internal-xy! g (fx x) (fy y) #\O)) ;; object is now one position left
					     stack))
			   ;; look at object to left
			   (loop (fx x) (fy y)))
			  
			  ;; empty space - activate the stack unwind			  
			  ((char=? ch #\.) (execute-stack stack) #t)
			  (#t
			   (fmt #t "unrecognised char ~a in grid at ~a ~a ~%" ch x y)
			   (error "step"))))))))
	     (execute-stack (lambda (xs)
			      (cond
			       ((null? xs) xs)
			       (#t (let ((fn (car xs)))
				     (fn)
				     (execute-stack (cdr xs))))))))
      ;; debug
      ;;(fmt #t "step : robot is at ~a ~a ~%" rx ry)
      ;; robot will always be able to move left
      ;; add that move to the stack
      (set! stack (cons (lambda ()
			  (internal-xy! g rx ry #\.) ;; clear space occupied by robot
			  (internal-xy! g (fx rx) (fy ry) #\@) ;; robot is now one position left
			  ;;(fmt #t "changing robot position from ~a ~a to " rx ry)
			  (set! rx (fx rx)) ;; change global position of robot
			  (set! ry (fy ry))
			  ;;(fmt #t " =>  ~a ~a ~%" rx ry)

			)
			stack))
      (loop (fx rx) (fy ry)))))


;; shows grid g 
(define (show-grid)
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
	     ((char=? ch #\@) (fmt #t "~a" #\@))
	     ((char=? ch #\.) (fmt #t "~a" #\.))
	     ((char=? ch #\O) (fmt #t "~a" #\O))
	     ((char=? ch #\#) (fmt #t "~a" #\#))	     
	     (#t (fmt #t "?~a?" ch))))
	  (loop-x (+ x 1)))))
      (loop-y (+ y 1))))))


(define (score-grid)
  (let ((score 0))
    (letrec ((foo (lambda (x y)
		    (cond
		     ((>= y hgt) score)
		     ((>= x wid) (foo 0 (+ y 1)))
		     (#t (let ((ch (internal-xy g x y)))
			   (cond
			    ((char=? ch #\O)
			     (set! score (+ score (* 100 y) x))))
			   (foo (+ x 1) y)))))))
      (foo 0 0))))








(define (reset)
  (set! example1 (read-example "../example1.txt"))
  (set! example2 (read-example "../example2.txt"))
  (set! input1 (read-example "../input.txt"))
  ;; choose your weapon 
  (set! problem input1)
  (set! g (to-array (find-grid problem)))
  (set! r (find-robot g))
  ;; (fmt #t "robot is at ~a ~%" r)
  (set! rx (car r))
  (set! ry (car (cdr r)))
  (set! wid (second (assoc 'wid g)))
  (set! hgt (second (assoc 'hgt g)))
  (set! wid-1 (- wid 1))
  (set! hgt-1 (- hgt 1))
  (set! moves (find-moves problem))
)  


(define (run)
  (reset)
  (fmt #t "running ........~%")
  ;; know where robot is at all times using rx ry
  (let ((count -1))
    (letrec ((next (lambda (xs)
		     (set! count (+ count 1))
		     ;;(fmt #t "count [~a] ~%" count)
		     ;;(show-grid)
		     (cond
		      ((null? xs) #t)
		      (#t (let ((ch (car xs)))			    
			    ;;(fmt #t " : move [~a] ~%" ch)			    
			    (cond
			     ((char=? ch #\<) (step-left))
			     ((char=? ch #\>) (step-right))
			     ((char=? ch #\^) (step-up))
			     ((char=? ch #\v) (step-down))
			     (#t (fmt #t "run next ch ~a : error bad char~%" ch)
				 (error "next")))
			    (next (cdr xs))))))))
      (next moves)
      (let ((result (score-grid)))
	(fmt #t "score = ~a ~%" result)
	result))))





	       


#|
think array may not be best approach here

(define g (to-array (find-grid example2)))
(internal-xy g 2 2)
(internal-xy g 2 4)
(internal-xy g 3 1)
;;(internal-xy! g 3 1 #\t)
(pp g )
;;(internal-xy! g 0 0 #\s)
(pp g )
(define gm (find-moves example2))

#\# is a wall character
#\. is empty square
#\O is something to move like a box
#\@ is the robot

we always start with robot x , y 
look at move  direction < left   right >   ^up  down v
depednign on the direction we look to see what implications of making the move

robot to go left
robot next position is (x-1,y)
look at (x-1,y) and ask is there push back - ie refusal to move ?
if #\# wall character then there is pushback , simply move onto next move , nobody moves
if #\. empty no pushback , everything mentioned up till now gets to move
if O object , then it will also ask neighbour in direction of move if that too can move ?
builds up a stack , if at end we find we can make the moves , then the stack is unwound with moves applied
and when stack is empty , next move can be applied


|#













