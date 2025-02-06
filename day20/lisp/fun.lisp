#|

quite tricky

cant see how get those savings listed in example

find route using breadth first search keeps a list of locations
- todo is current list of locations we explore from up / down / left / right
- if find a square that has no value assigned and is an empty square #\. . dot char then we add that square to
- the later list

repeat loop
- todo becomes later list
- later list becomes empty




|#

(defpackage :aoc
  (:use :cl))

(in-package :aoc)

(declaim (optimize (speed 0)(debug 3)(safety 3)(space 0)))


;; a fixed square grid
;; how should we gracefully handle errors ? access out of bounds on X Y ?

;; defstruct - a bit overly done to be honest with macros 
(defstruct grid
   (width 1)
   (height 1)
   (limit 1) ;; row major limit is 0 to limit-1 
   (array #() :type simple-vector))


;; position already defined
;; (defstruct position
;;   (x 0)
;;   (y 0))

(defstruct coord
  (x 0)
  (y 0))


;; convert this into some sort of 2d grid 
(defparameter example-string
"###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

;; read it from a file ?
;;
(defun read-from-file (file)
  (with-open-file (stream file :direction :input)
    (let ((lines nil))
      (catch 'done
	(loop while t do
	  (let ((line (read-line stream nil 'eof)))
	    (cond
	      ((eq line 'eof) (throw 'done t))
	      ((zerop (length line)) 'ignore-blank-lines)
	      (t (setq lines (cons line lines)))))))
      (setq lines (reverse lines))
      (setq lines (mapcar (lambda (x) (concatenate 'string x (format nil "~a" #\newline))) lines))
      lines)))


;; 
(defun grid-from-file (file)
  (str-to-grid (eval `(concatenate 'string ,@(read-from-file file)))))



;; valid chars S E . # anything else is 
(defun str-to-grid (s)
  (let ((slen (length s))
	(arr nil)
	(x 0)(y 0)(j 0)(width 0)(height 0)(hash (make-hash-table :test 'equalp)))
    (loop for i from 0 to (- slen 1) do
      (let ((ch (char s i)))
	(cond
	  ((char= ch #\newline)
	   ;; (format t "newline!!~%")
	   (incf y)
	   (setq x 0))
	  (t 
	   ;; (format t "(~a ,~a) => ~a : ~a ~%" x y j ch)
	   (setf (gethash (list x y) hash) j)
	   (incf j)
	   (when (>= x width) (setq width (+ x 1)))
	   (when (>= y height) (setq height (+ y 1)))
	   (incf x)))))

    ;; (format t "~%~%Round Two~%~%")
    ;; (format t "width = ~a : height =~a ~%" width height)    
    
    ;; round 2
    (setq x 0)
    (setq y 0)
    (setq j 0)
    (setq arr (make-array (* width height) :initial-element #\# ))
    ;; height width fixed 
    (loop for i from 0 to (- slen 1) do
      (let ((ch (char s i)))
	(cond
	  ((char= ch #\newline)
	   ;; (format t "newline!!~%")
	   (incf y)
	   (setq x 0))
	  (t 
	   ;;(format t "~a (~a ,~a) ~a ~%" j x y ch)
	   (let ((id (gethash (list x y) hash))
		 (id2 (+ x (* y width))))
	     (setf (aref arr id2) ch)
	     ;; (format t "(~a ,~a) => ~a vs ~a ~%" x y id id2)
	     (when (/= id id2)
	       (format t "errr id /= id2  ~a ~a : (x~a , y~a) ~%" id id2 x y)
	       (error "id /= id2"))	       
	     (incf j)
	     (incf x))))))
    (let ((g (make-grid :width width :height height :limit j :array arr)))
      g)))



(defun grid-xy (g x y)
  (let ((lim-x (grid-width g))
	(lim-y (grid-height g)))
    (cond
      ((or (< x 0) (>= x lim-x)) (error "grid-xy out bounds on x"))
      ((or (< y 0) (>= y lim-y)) (error "grid-xy out bounds on y"))
      (t (aref (grid-array g) (+ x (* y (grid-width g))))))))

(defun grid-xy! (g x y z)
  (let ((lim-x (grid-width g))
	(lim-y (grid-height g)))
    (cond
      ((or (< x 0) (>= x lim-x)) (error "grid-xy out bounds on x"))
      ((or (< y 0) (>= y lim-y)) (error "grid-xy out bounds on y"))
      (t (setf (aref (grid-array g) (+ x (* y (grid-width g)))) z)))))




;; use row major order 
(defun grid-show (g)
  (format t "~%")
  (let* ((wid (grid-width g))
	 ;; (hgt (grid-height g))
	 ;; (lim (* wid hgt))
	 (lim2 (grid-limit g))
	 (arr (grid-array g))
	 (x 0)
	 (y 0))
    (loop for i from 0 to (- lim2 1) do
      (format t "~3a " (aref arr i))
      (incf x)
      (when (>= x wid)
	(setq x 0)
	(incf y)
	(format t "~%")))))

;; will copy elements if they are simple values
;; if they are complex types a more indepth deep copy will be needed
(defun grid-copy (g)
  (let* ((width (grid-width g))
	 (height (grid-height g))
	 (limit (grid-limit g))
	 (array (grid-array g))
	 (array2 (make-array limit :initial-element #\#)))
    (loop for i from 0 to (- limit 1) do
      (setf (aref array2 i) (aref array i)))
    (make-grid :width width :height height :limit limit :array array2)))



(defparameter g (make-grid :width 2 :height 2 :array #(1 2 3)))
(defparameter g2 (str-to-grid
		  "***
*.*
.*.
*.*
***"))
(defparameter g3 (str-to-grid example-string))
(defparameter g4 (grid-copy g3))

(defparameter ginput (grid-from-file "../input.txt"))
(defparameter gexample (grid-from-file "../example.txt"))


;; find start S and end E positions

(defun find-start-end (g)
  (let* ((width (grid-width g))
	 (height (grid-height g))
	 (x 0)(y 0)
	 (nstart 0)
	 (nend 0)
	 (start nil)
	 (end nil))
    (loop for y from 0 to (- height 1) do
      (loop for x from 0 to (- width 1) do
	(let ((ch (grid-xy g x y)))
	  (when (char= ch #\S)
	    (incf nstart)
	    (setq start (make-coord :x x :y y)))
	  (when (char= ch #\E)
	    (incf nend)
	    (setq end (make-coord :x x :y y))))))
    (when (not (= nstart 1)) (error "not enough starts"))
    (when (not (= nend 1)) (error "not enough starts"))
    (values start end)))


;; is such a square safe to poke without an error being spewed    
(defun safe? (g x y)
  (let* ((width (grid-width g))
	 (height (grid-height g)))
    (cond
      ((< x 0) nil)
      ((>= x width) nil)
      ((< y 0) nil)
      ((>= y height) nil)
      (t t))))

(defun wall? (g x y)
  (and (safe? g x y)
       (let ((val (grid-xy g x y)))
	 (cond
	   ((equalp #\# val) t)
	   (t nil)))))
       

(defun empty? (g x y)
  (and (safe? g x y)
       (let ((val (grid-xy g x y)))
	 (cond
	   ((equalp #\. val) t)
	   (t nil)))))


(defun grid-set! (g p val)
  (let ((x (coord-x p))
	(y (coord-y p)))
    (cond
      ((safe? g x y) (grid-xy! g x y val))
      (t nil))))

(defun grid-get (g p)
  (let ((x (coord-x p))
	(y (coord-y p)))
    (cond
      ((safe? g x y) (grid-xy g x y))
      (t nil))))


(defun coord-wall? (g p)
  (let ((x (coord-x p))
	(y (coord-y p)))
	 (wall? g x y)))
 
(defun coord-empty? (g p)
  (let ((x (coord-x p))
	(y (coord-y p)))
    (empty? g x y)))


(defun coord-safe? (g p)
  (let ((x (coord-x p))
	(y (coord-y p)))
    (safe? g x y)))


(defun coord-up (p)
  (let* ((x (coord-x p))
	 (y (coord-y p))
	 (x2 x)
	 (y2 (- y 1)))
    (make-coord :x x2 :y y2)))

(defun coord-down (p)
  (let* ((x (coord-x p))
	 (y (coord-y p))
	 (x2 x)
	 (y2 (+ y 1)))
    (make-coord :x x2 :y y2)))

(defun coord-left (p)
  (let* ((x (coord-x p))
	 (y (coord-y p))
	 (x2 (- x 1))
	 (y2 y))
    (make-coord :x x2 :y y2)))

(defun coord-right (p)
  (let* ((x (coord-x p))
	 (y (coord-y p))
	 (x2 (+ x 1))
	 (y2 y))
    (make-coord :x x2 :y y2)))






;; expanding incremental move from current position until we reach end coord
;; if we have a todo stack 
(defun racetrack (g-orig)
  (let* ((g (grid-copy g-orig))
	 (width (grid-width g))
	 (height (grid-height g))
	 (progress 0)
	 (todo nil)
	 (later nil)
	 (here nil))
    (multiple-value-bind (start end) (find-start-end g)
      (let ((n 0))
	
	(grid-set! g end #\.)
	(grid-set! g start 0)
	(setq n -1)

	(setq todo nil)
	(setq later (list start))
	
	;; progress is loop condition
	(setq progress 1)
	;;(loop while (> progress 0) do
	(loop while (not (null later)) do
	  ;; stop looping if made no more progress ...
	  (setq progress 0)
	  ;;
	  (incf n)

	  ;; move later into the todo pile 	  ;; clear the later pile
	  (setq todo later)
	  (setq later nil)

	  ;;(format t "there are ~a positions in the todo pile~%" (length todo))
	  ;; any new square to be visited will be added to the later pile
	  ;; display each iteration 
	  ;;(grid-show g)	  
	  ;; look for any square with a integer value of n and ask
	  ;; can we go up ? is up safe AND is up empty - ie not a wall
	  ;; loop for x from 0 to (- width 1) do
	  ;;   (loop for y from 0 to (- height 1) do
	  (dolist (here todo)
	      (let* ((up (coord-up here))
		     (down (coord-down here))
		     (left (coord-left here))
		     (right (coord-right here)))
		;; if here contains an integer val
		(when (coord-safe? g here)
		  (let ((val (grid-get g here)))
		    (when (integerp val) ;;(and (integerp val) (= val n))
				      
		      (when (and (coord-safe? g up) (coord-empty? g up))
			(grid-set! g up (+ val 1))
			(setq later (cons up later))
			(incf progress))
				      
		      (when (and (coord-safe? g down) (coord-empty? g down))
			(grid-set! g down (+ val 1))
			(setq later (cons down later))
			(incf progress))
				      
		      (when (and (coord-safe? g left) (coord-empty? g left))
			(grid-set! g left (+ val 1))
			(setq later (cons left later))			
			(incf progress))
				      
		      (when (and (coord-safe? g right) (coord-empty? g right))
			(grid-set! g right (+ val 1))
			(setq later (cons right later))			
			(incf progress))
		      ))))))
			      
	  ;; if no more progress
    (let ((n-end (grid-get g end)))
;;      (format t "the end counter is ~a~%" n-end)
      (values n-end g))))))





;; 2 squares can be compromised such that neither x = 0 y = 0 x = (wid-1) y = (hgt - 1)
;;  x = (wid - 1) is the border of the grid , no point compromising there as it does not lead to any advantage
;;
;;       x ?
;;       ?
;; osol is original racetrack solution 
;;
(defmacro %enter-into-hash% (val)
  `(let ((has (gethash ,val hash nil)))
     (cond
       ((null has) (setf (gethash ,val hash) 1))
       (t (incf (gethash ,val hash))))))


(defun racetrack-cheat (g)
  (let* ((width (grid-width g))
	 (height (grid-height g))
	 (hash (make-hash-table :test 'equalp))
	 (osol (racetrack g)))    
    
    (multiple-value-bind (start end) (find-start-end g)
      (loop for x from 1 to (- width 1) do
	(loop for y from 1 to (- height 1) do
	  
	  ;; has to be a connected square
	  (let* ((c1 (make-coord :x x :y y))
		 (c2 (make-coord :x (+ x 1) :y y))
		 ;;(c3 (make-coord :x (- x 1) :y y))
		 ;;(c4 (make-coord :x x :y (- y 1)))
		 (c5 (make-coord :x x :y (+ y 1))))
	    
	    (when (and (coord-safe? g c1) (coord-safe? g c2))
	      (let ((g2 (grid-copy g)))
		(grid-set! g2 c1 #\.)
		(grid-set! g2 c2 #\.)
		(grid-set! g2 start #\S)
		(grid-set! g2 end #\E)
		(multiple-value-bind (r gout) (racetrack g2)
		  (format t "cheat (c2) at ~a ~a -> ~a : saving of ~a ~%"c1 c5 r (abs (- osol r)))
		  (let ((saving (abs (- osol r))))
		    (when (= saving 64) (format t "saving of 64 !!!~%"))
		    (%enter-into-hash% saving)
		  ))))

	    #|
	    (when (and (coord-safe? g c1) (coord-safe? g c3))
	      (let ((g2 (grid-copy g)))
		(grid-set! g2 c1 #\.)
		(grid-set! g2 c3 #\.)
		(grid-set! g2 start #\S)
		(grid-set! g2 end #\E)
		(let ((r (racetrack g2)))
		  ;;(format t "cheat at ~a ~a -> ~a ~%"c1 c3 r)
		  (format t "cheat (c3) at ~a ~a -> ~a : saving of ~a ~%"c1 c5 r (abs (- osol r)))
		  (let ((saving (abs (- osol r))))
		    (%enter-into-hash% saving)))))

	    (when (and (coord-safe? g c1) (coord-safe? g c4))
	      (let ((g2 (grid-copy g)))
		(grid-set! g2 c1 #\.)
		(grid-set! g2 c4 #\.)
		(grid-set! g2 start #\S)
		(grid-set! g2 end #\E)
		(let ((r (racetrack g2)))
;;		  (format t "cheat at ~a ~a -> ~a ~%"c1 c4 r)
		  (format t "cheat (c4) at ~a ~a -> ~a : saving of ~a ~%"c1 c5 r (abs (- osol r)))
		  (let ((saving (abs (- osol r))))
		    (%enter-into-hash% saving)))))
	    |#
	    
	    (when (and (coord-safe? g c1) (coord-safe? g c5))
	      (let ((g2 (grid-copy g)))
		(grid-set! g2 c1 #\.)
		(grid-set! g2 c5 #\.)
		(grid-set! g2 start #\S)
		(grid-set! g2 end #\E)
		(let ((r (racetrack g2)))
		  (format t "cheat (c5) at ~a ~a -> ~a : saving of ~a ~%"c1 c5 r (abs (- osol r)))
		  (let ((saving (abs (- osol r))))
		    (when (= saving 64) (format t "saving of 64 !!!~%"))
		    (%enter-into-hash% saving)))))
	    
	    ))))
    (maphash (lambda (k v)
	       (format t "The value associated with the key ~S is ~S~%" k v)) hash)))

	    

	    

	    

	  





		      ;; debug
		      ;; (when (= val 32)
		      ;; 	(let ((e (coord-empty? g left)))
		      ;; 	  (format t "e => ~a ~%" e)))
		      
		      ;; ;; <-- up  -->
		      ;; (let ((val2 (grid-get g up)))
		      ;; 	(when (integerp val2)
		      ;; 	  (cond
		      ;; 	    ((= (+ val 1) val2) 'expected)
		      ;; 	    (t ;;(format t "grid vals differ here:~a up:~a ~%" val val2)
		      ;; 	     nil
		      ;; 	       ))))
		  

		      ;; <-- down  -->
		      ;; (let ((val2 (grid-get g down)))
		      ;; 	(when (integerp val2)
		      ;; 	  (cond
		      ;; 	    ((= (+ val 1) val2) 'expected)
		      ;; 	    (t ;;(format t "grid vals differ here:~a down:~a ~%" val val2)
		      ;; 	     nil
		      ;; 	       ))))


		      ;; <-- right  -->
		      ;; (let ((val2 (grid-get g right)))
		      ;; 	(when (integerp val2)
		      ;; 	  (cond
		      ;; 	    ((= (+ val 1) val2) 'expected)
		      ;; 	    (t ;;(format t "grid vals differ here:~a down:~a ~%" val val2)
		      ;; 	     nil
		      ;; 	       ))))


		      	  

		      
		      ;; <-- left  -->
		      ;; (let ((val2 (grid-get g left)))
		      ;; 	(when (integerp val2)
		      ;; 	  (cond
		      ;; 	    ((= (+ val 1) val2) 'expected)
		      ;; 	    (t ;;(format t "grid vals differ here:~a down:~a ~%" val val2)
		      ;; 	     nil
		      ;; 	       ))))


		;; -> if up is safe -> and is an empty square #\. -> place (val + 1) here
		;; -> if down is safe -> and is an empty square #\. -> place (val + 1) here
		;; -> if left is safe -> and is an empty square #\. -> place (val + 1) here
		;; -> if right is safe -> and is an empty square #\. -> place (val + 1) here
		;;
		;; ?? if value stored at non empty square is larger than (val + 1)
		;; ?? we should replace it with (val + 1)
		;; ?? can this ever occur in practice ??
		;;
		;;   increment progress whenever we place a (val + 1)
		;;   informs us we made some progress and keep iterating
		;; when we finish all looping and no more progress made ,
		;; we should 



	    
	  ;; have we reached the end yet ?
	  
	
      
    ;; up
    ;; left
    ;; right
    ;; down
    ;; do we stay on the map ?
    ;; do we hit a wall 
    








 

















