


(import (chicken format))
(import (chicken pretty-print))
(import (srfi-1))
(import (simple-loops))
(define fmt format)

;;(do-list (a '(a b c)) (fmt #t "~a~%" a))

#|

keypad 1 
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A | 
    +---+---+

1 direct control of keypad 1 - all different ways to go from initial starting point A bottom right

keypad 2
    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+

2 uses keypad2 to control 1
3 uses keypad2 to control 2
4 uses keypad2 to control 3



|#

;; some position data type 
(define make-pos
  (lambda (x y)
    (let ((result (vector x y)))
      result)))

(define pos-x
  (lambda (v)
    (vector-ref v 0)))

(define pos-y
  (lambda (v)
    (vector-ref v 1)))

(define pos-same?
  (lambda (v v2)
    (and (= (pos-x v) (pos-x v2))
	 (= (pos-y v) (pos-y v2)))))

(define (pos-left p)
  (let ((x (pos-x p))
	(y (pos-y p)))
    (make-pos (- x 1) y)))

(define (pos-right p)
  (let ((x (pos-x p))
	(y (pos-y p)))
    (make-pos (+ x 1) y)))

(define (pos-down p)
  (let ((x (pos-x p))
	(y (pos-y p)))
    (make-pos x (+ y 1))))

(define (pos-up p)
  (let ((x (pos-x p))
	(y (pos-y p)))
    (make-pos x (- y 1))))



;; find a shortest path between two squares such we never pass through square 1 4
;; shortest path means we do step on square already visited - progress
(define find-path-1
  (let ((p14 (make-pos 1 4)))
    (lambda (start end)
      (let ((best '())
	    (best-len #f))
	(letrec
	    ((find-path-helper
	      (lambda (p t seen move len)
		;;(format #t "at ~a ~%" p)
		(cond
		 ((< (pos-x p) 1) #f) ;; keep robot on keyboard 
		 ((> (pos-x p) 3) #f)
		 ((< (pos-y p) 1) #f)
		 ((> (pos-y p) 4) #f)
		 ((pos-same? p p14) #f) ;; cannot go to p14 square
		 ((member p seen) #f) 
		 ((pos-same? p t)
		  ;;(format #t "~a in ~a with len ~a~%moves ~a~%" p seen len move)
		  (cond
		   ((or (eq? best-len #f) (< len best-len))
		    (set! best-len len)
		    (set! best (list (list len (reverse move)))))
		   ((<= len best-len)
		    (set! best-len len)
		    (set! best (cons (list len (reverse move)) best)))))		
		 (#t (find-path-helper (pos-left p) t (cons p seen) (cons '< move) (+ len 1))
		     (find-path-helper (pos-right p) t (cons p seen) (cons '> move) (+ len 1))
		     (find-path-helper (pos-down p) t (cons p seen) (cons 'v move) (+ len 1))
		     (find-path-helper (pos-up p) t (cons p seen) (cons '^ move) (+ len 1)))))))
	  (let ((len 0)
		(seen '())
		(move '()))	      
	    (find-path-helper start end seen move len)))
	best))))
  
;; X Y   top left = 1 1 

;; 7 coord 1 1
;; 8       2 1
;; 9       3 1
;; 4       1 2
;; 5       2 2
;; 6       3 2
;; 1       1 3
;; 2       2 3
;; 3       3 3
;; 0       2 4
;; A       3 4
;; avoid square 1 4

(define (same? x s c i)
  (or (and (symbol? x) (eq? x s))
      (and (integer? x) (= x i))
      (and (char? x) (char=? x c))))




;; 7 8 9
;; 4 5 6
;; 1 2 3
;; x 0 A
(define (keypad->pos n)
  (cond
   ((same? n '7 #\7 7) (make-pos 1 1))
   ((same? n '8 #\8 8) (make-pos 2 1))
   ((same? n '9 #\9 9) (make-pos 3 1))
   
   ((same? n '4 #\4 4) (make-pos 1 2))
   ((same? n '5 #\5 5) (make-pos 2 2))
   ((same? n '6 #\6 6) (make-pos 3 2))
   
   ((same? n '1 #\1 1) (make-pos 1 3))
   ((same? n '2 #\2 2) (make-pos 2 3))
   ((same? n '3 #\3 3) (make-pos 3 3))
   
   ((same? n '0 #\0 0) (make-pos 2 4))
   ((same? n 'a #\a 0) (make-pos 3 4))
   ((same? n 'A #\A 0) (make-pos 3 4))     
     
   (#t (error "keypad->pos bad n"))))

   

(define (keypad-type s)
  (let* ((xs (string->list s))
	 (points (map keypad->pos xs)))
    (cons (keypad->pos 'A) points)))


;; (define (foldup xs)
;;   (letrec ((foldup2 (lambda (p xs)
;; 		      (cond
;; 		       ((null? xs) '())
;; 		       ((null? (cdr xs))
;; 			(list (list p (car xs))))
;; 		       (#t (let ((first (car xs))	    
;; 				(rest (cdr (cdr xs))))
;; 			    (cons (list p first)
;; 				  (foldup2 first rest))))))))
;;   (cond
;;    ((null? xs) '())
;;    (#t (let ((first (car xs))
;; 	    (second (car (cdr xs)))
;; 	    (rest (cdr (cdr xs))))
;; 	(cons (list first second)
;; 	      (foldup2 second rest)))))))

(define (foldup xs)
  (cond
   ((null? xs) '())
   ((null? (cdr xs)) '())
   (#t (cons (take xs 2) (foldup (cdr xs))))))


;; may be N different key presses - each movement from start to [1] will have A ways to do it
;; from [1] to [2] will have B ways to do it
;; ... [2] to [3] then 
;; ... [3] to [4]
;;  ...[P] to [Z] finally 
;; [1] [2] [3] [4] [5] [6] [7] 
(define (aseq str fn)
  (let* ((motions (map (lambda (x)
			 (let ((from (car x))
			       (to (car (cdr x))))
			   (find-path-1 from to)))
		       (let ((points (keypad-type str)))
			 (foldup points)))))
    (activate motions '() fn)))

(define (activate xs moves fn)
  (cond
   ((null? xs) (fn (apply append (reverse moves))))
   (#t (let ((a (car xs)))
	;; a is a collection of ways to get from A to B on keypad 1
	(do-list (id-move a)
		 (let ((move (second id-move)))
		   ;;(fmt #t "move = ~a~%" move)
		   (activate (cdr xs) (cons (list 'A) (cons move moves)) fn)))))))




       

;; how do visualize what machine is trying to do
;; debug - reason about what it is doing


(define (test)
  (let ((start (make-pos 3 4))
	(end (make-pos 2 4)))
    (find-path-1 start end)))



#|
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A | <- robot 1 currently at lower right corner
    +---+---+
|#


(define (robot-1 s fn)
  (aseq s fn))


(define (test-robot-1)
  (robot-1 "029A"  (lambda (x) (fmt #t "motions required are ~a~%" x))))








#|

directional keypad starts at top right A 

 1    2   3
    +---+---+  
    | ^ | A |   1
+---+---+---+
| < | v | > |   2
+---+---+---+

|#


(define (keypad2->pos s)
  (cond
   ((eq? s '^) (make-pos 2 1))
   ((or (eq? s 'A) (eq? s 'a)) (make-pos 3 1))
   
   ((eq? s '<) (make-pos 1 2))
   ((eq? s 'v) (make-pos 2 2))
   ((eq? s '>) (make-pos 3 2))
   (#t (error (fmt #f "bad keypad2 position ~a" s)))))



;; now working with directional keypad
;; height > 2 false
;; height < 1 false
;; width < 1 false
;; width > 3 false
;; x = 1 and y = 1 then false as pointing at empty gap
(define find-path-2
  (let ((pNo (make-pos 1 1)))
    (lambda (start end)
      (let ((best '())
	    (best-len #f))
	(letrec
	    ((find-path-helper
	      (lambda (p t seen move len)
		;;(format #t "at ~a ~%" p)
		(cond
		 ((< (pos-x p) 1) #f) ;; keep robot on keyboard 
		 ((> (pos-x p) 3) #f)
		 ((< (pos-y p) 1) #f)
		 ((> (pos-y p) 2) #f)
		 ((pos-same? p pNo) #f) ;; cannot go to thissquare
		 ((member p seen) #f) 
		 ((pos-same? p t)
		  ;;(format #t "~a in ~a with len ~a~%moves ~a~%" p seen len move)
		  (cond
		   ((or (eq? best-len #f) (< len best-len))
		    (set! best-len len)
		    (set! best (list (list len (reverse move)))))
		   ((<= len best-len)
		    (set! best-len len)
		    (set! best (cons (list len (reverse move)) best)))))		
		 (#t
		  ;; (fmt #t "we are at ~a ~%" p )
		  (find-path-helper (pos-left p) t (cons p seen) (cons '< move) (+ len 1))
		  (find-path-helper (pos-right p) t (cons p seen) (cons '> move) (+ len 1))
		  (find-path-helper (pos-down p) t (cons p seen) (cons 'v move) (+ len 1))
		  (find-path-helper (pos-up p) t (cons p seen) (cons '^ move) (+ len 1)))))))
	  (let ((len 0)
		(seen '())
		(move '()))	      
	    (find-path-helper start end seen move len)))
	best))))



(define (test2)
  (let ((start (keypad2->pos 'a))
	(end (keypad2->pos 'v)))
    (find-path-2 start end)))




#|
#;674> (robot-1 "029" (lambda (x) (fmt #t "~a~%" x)))
(< A ^ A > ^ ^ A)
(< A ^ A ^ > ^ A)
(< A ^ A ^ ^ > A)

currently tell robot 1 - type 029 then i get a series of solutions passed back to my fn
that i supply

x will be a series of moves i need to make

robot-2 is currently at square 'A' (keypad2->pos 'a)
figure out shortest paths to 

robot-2 starts at (keypad2->pos 'a) 

|#

;; top level - run simulation , now available for inspection makes debugging easier
(define robot-2-best #f)
(define robot-2-solutions '())
(define robot-2-all '())

(define (robot-2 s fn)
  (robot-1 s
	   (lambda (robot-1-moves)
	     (let ((from 'a))
	       (robot-2-b from robot-1-moves '() fn))))
  (fn robot-2-solutions))

;; acc accumulates moves in reverse order , when no moves to do - call fn
(define (robot-2-b from moves acc fn)
  (cond
   ((null? moves)
    ;; (format #t "calling fn with args ~%~a~%" (apply append (reverse acc)))
    ;; (format #t "fn ~a~%" fn)    
    (robot-2-c (apply append (reverse acc))))
   (#t (let* ((to (car moves))
	      (paths (find-path-2 (keypad2->pos from) (keypad2->pos to))))
	 ;; (format #t "possible paths = ~a~%" paths)
	 (do-list (id-path paths)
		  (let ((path (second id-path)))
		    ;; (format #t "choosing path = ~a~%" path)
		    (robot-2-b to (cdr moves) (cons (list 'A) (cons path acc)) fn)))))))


(define robot-2-c
  (lambda (sol)
    (let ((slen (length sol)))
      (set! robot-2-all (cons sol robot-2-all))
      (when (not robot-2-best)	(set! robot-2-best slen))
      (cond
       ((< slen robot-2-best)
	(set! robot-2-solutions (list sol))
	(set! robot-2-best slen))
       ((= slen robot-2-best)
	(set! robot-2-solutions (cons sol robot-2-solutions)))
       (#t #f)))))


(define (test-robot-2 s)
  (set! robot-2-best #f)
  (set! robot-2-solutions '())
  (set! robot-2-all '())
  (robot-2 s (lambda (xs)
	       (map (lambda (z)
		      (fmt #t "soln : ")
		      (map (lambda (y) (fmt #t "~a" y)) z)
		      (fmt #t "~%"))	      
		    xs)
	       #f)))



;; top level - run simulation , now available for inspection makes debugging easier
(define robot-3-best #f)
(define robot-3-solutions '())
(define robot-3-all '())

(define (robot-3 s fn)
  (robot-2 s
	   (lambda (robot-2-moves)	     
	     (let ((from 'a))
	       (do-list (r2m robot-2-moves)
			(robot-3-b from r2m '() fn)))))
  (fn robot-3-solutions))


;; acc accumulates moves in reverse order , when no moves to do - call fn
(define (robot-3-b from moves acc fn)
  (cond
   ((null? moves)
    (robot-3-c (apply append (reverse acc))))
   (#t (let* ((to (car moves))
	      (paths
	       (begin 
		 ;; (format #t "from = ~a~%" from)
		 ;; (format #t "to = ~a~%" to)
		 (find-path-2 (keypad2->pos from) (keypad2->pos to)))))
	 ;; (format #t "possible paths = ~a~%" paths)
	 (do-list (id-path paths)
		  (let ((path (second id-path)))
		    ;; (format #t "choosing path = ~a~%" path)
		    (robot-3-b to (cdr moves) (cons (list 'A) (cons path acc)) fn)))))))



(define robot-3-c
  (lambda (sol)
    (let ((slen (length sol)))
      (set! robot-3-all (cons sol robot-3-all))
      (when (not robot-3-best)	(set! robot-3-best slen))
      (cond
       ((< slen robot-3-best)
	(set! robot-3-solutions (list sol))
	(set! robot-3-best slen))
       ((= slen robot-3-best)
	(set! robot-3-solutions (cons sol robot-3-solutions)))
       (#t #f)))))

(define (list->out xs)
  (map (lambda (ch) (format #t "~a" ch)) xs)
  #f)

(define (test-robot-3 s)
  (set! robot-2-best #f)
  (set! robot-2-solutions '())
  (set! robot-2-all '())

  (set! robot-3-best #f)
  (set! robot-3-solutions '())
  (set! robot-3-all '())
  
  (robot-3 s (lambda (xs)
	       (map (lambda (x)
		      (fmt #t "soln : ")
		      (list->out x)
		      (fmt #t "~%"))
		    xs))))







	       


