


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


;; (bind (x y) '(1 2) x)
;; (bind (x y) '(1 2) y)


(define ppe (lambda (x) (pp (expand* x))))
;; %%%%%%%%% macros %%%%%%%%%%%%%%%%%%

(define-macro (href h k)
  `(hash-table-ref ,h ,k))

(define-macro (href/d h k d)
  `(hash-table-ref/default ,h ,k ,d))


(define-macro (hset! h k v)
  `(hash-table-set! ,h ,k ,v))



;; %%%%%%%%%%%%%%%%%%%%%%%%%55

(define (pad4 s)  
  (let ((len (string-length s)))
    (cond
     ((= len 1) (format #f "   ~a" s))
     ((= len 2) (format #f "  ~a" s))
     ((= len 3) (format #f " ~a" s))
     ((= len 4) (format #f "~a" s))
     (#t s))))



;; mix 64
;; subtract 60 and add 4
;; just subtract 56 then in one step

;; prune
;;(define prune  (lambda (n)  (modulo n 16777216)))
;;(prune 100000000)

(define-macro (swap! x y) ; wrong
  `(let ((tmp ,x))
      (set! ,x ,y)
      (set! ,y tmp)))

(let ((a 1)(b 2))
  (swap! a b)
  (list a b))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

(define list-grids (split-lines lines))


(define grids (list->vector (cons #f list-grids)))



;; if want 1 1 based grid access

;; y is 1 based 
(define (grid-row g y)
  (list-ref g (- y 1)))

;; x y is 1 1 based 
(define (grid-xy g x y)
  (string-ref (grid-row g y) (- x 1)))

;; (list-ref '(a b c d) 0)

(let ((g '("ABCDE" "#####" "#.###" "#.###" "#..##" "....#" "VWXYZ")))
  (grid-xy g 1 1)
  (grid-xy g 2 1)
  (grid-xy g 1 7)
  (grid-xy g 2 7)
  (grid-xy g 5 7)  
  )

(define (grid n)
  (vector-ref grids n))

(define (grid-rows g)
  (length g))

(define grid-count (- (vector-length grids) 1))

(define (grid-width g)
  (string-length (car g)))

(define (grid-height g)
  (length g))


(grid 1)
(grid-rows (grid 1))
grid-count
(grid grid-count) ;;  ("#####" "#####" "#####" "#.#.#" "#.#.." "..#.." ".....")

;; have a grid representation very slow , but who cares for now

; this is so convoluted and inefficient , how do they do 
(define (search g ch)
  (let ((rows g)
	(width (grid-width g))
	(x #f)(y 0))  
    (letrec ((search-row
	      (lambda (s i)
		(cond
		 ((> i width) #f)
		 (#t (let ((c (string-ref s (- i 1))))
		       (cond
			((char=? c ch) i) ;; found it
			(#t (search-row s (+ i 1))))))))))
    (call/cc (lambda (escape)
	       (do-list (row rows)
			(set! y (+ y 1))
			(let ((out (search-row row 1)))
			  (when out
			    (set! x out)
			    (escape #t)
			    ;; next row
			    )))))
    (if x
	(list x y)
	#f))))


;; using a hash instead
(define hash (make-hash-table))

(define helper
  (lambda (g y)
    (cond
     ((null? g) hash)
     (#t (let ((str (car g)))
	   (fmt #t "str = ~a ~%" str)	   
	   (helper2 g str (string-length str) 1 y)
	   (helper (cdr g) (+ y 1)))))))

(define helper2
  (lambda (g s slen x y)
    (cond
     ((> x slen) #f)
     (#t (let ((c (string-ref s (- x 1))))
	   ;; do
	   (fmt #t "setting (~a ~a) -> ~a ~%" x y c)
	   (hash-table-set! hash (list x y) c)
	   (helper2 g s slen (+ x 1) y))))))

(define (g2hash g)
  (helper g 1)
  (hash-table-set! hash 'width (grid-width g))
  (hash-table-set! hash 'height (grid-height g)))
 

(define g (g2hash (grid 1)))

;; copy grid ?

(define (hack-hash h)
  (let ((keys (hash-table-keys h)))
    (do-list (key keys)
	     (let ((val (hash-table-ref h key)))
	       (when (char? val)
		 (cond
		  ((or (char=? #\S val) (char=? #\s val))
		   (hash-table-set! h key #\.)
		   (hash-table-set! h 'start key))
		  ((or (char=? #\E val) (char=? #\e val))
		   (hash-table-set! h key #\.)
		   (hash-table-set! h 'end key))))))))




;; copy grid
(define (copy-hash h)
  (let* ((tmp (make-hash-table))
	 (keys (hash-table-keys h)))
    (do-list (key keys)
	     (let ((val (hash-table-ref h key)))
	       (hash-table-set! tmp key val)))
    tmp))

(define (test-hash-copy)
  (let ((tmp2 (copy-hash hash))
	(tmp3 (copy-hash hash)))
    (hash-table-set! tmp2 'start '(6 4))
    (fmt #t "tmp2 start : ~a~%" (hash-table-ref tmp2 'start))
    (fmt #t "tmp3 start : ~a~%" (hash-table-ref tmp3 'start))
    (fmt #t "hash start : ~a~%" (hash-table-ref hash 'start))))

;; we can copy hash - yep
;; why dont we use lua instead
;; why isnt everything a hash table ?
;; think about more efficient implementations better user interface than
;; (hash-table-make-new-hash-table ...
;; (new)

;; start at start . place 0 there . find all places can reach from here.
;; start end walls
;; a wall #\# char
;; an empty #\. char
;; can move up / down / left or right
;; do not go to x = 1 y = 1 x = width or y = height
(define (show-hash h)
  (let ((start (href h 'start))
	(end (href h 'end))
	(width (href h 'width))
	(height (hash-table-ref h 'height))
	(cheat-1 (href/d h 'cheat-1 #f)) ;; not all grids have cheat-1 cheat-2 so provide a default value #f false
	(cheat-2 (href/d h 'cheat-2 #f)))
    (when (or cheat-1 cheat-2)
      (fmt #t "using cheats ~a : ~a ~%" cheat-1 cheat-2))	
    (fmt #t "width ~a height ~a ~%" width height)	
    (let loop ((x 1)(y 1))
      (cond
       ((> x width)
	(fmt #t "~%")	
	(loop 1 (+ y 1)))
       ((> y height) #f)
       (#t
	(let ((ch (hash-table-ref h (list x y))))
	  (fmt #t "~a" (pad4 (cond
			      ((or (equal? (list x y) cheat-1)
				   (equal? (list x y) cheat-2))
			       (format #f "!~a" ch))
			      ((equal? (list x y) start)
			       ;;(format #f "\x1b[1;31ms~a" ch))
			       (format #f "s~a" ch))
			      ((equal? (list x y) end)
			       (format #f "e~a" ch))			      
			      (#t (format #f "~a" ch)))))
	  (loop (+ x 1) y)))))))








(hack-hash hash)
(hash-table-ref hash 'start)
(hash-table-ref hash 'end)

(show-hash hash)



;; destructively modifies hash given 
(define (wander! h)
  (let* ((start (href h 'start))
	 (end (href h 'end))
	 (width (href h 'width))
	 (height (href h 'height))	 
	 (did-set #f))
    
    (define (go x y n) ;; try put an integer at x y where there is an empty #\. char 
      (cond
       ((<= x 1) #f)
       ((<= y 1) #f)       
       ((>= x width) #f)
       ((>= y height) #f)
       (#t (let ((ch (href h (list x y))))
	     (cond
	      ((and (char? ch) (char=? ch #\.)) ;; empty square - place integer there
	       (set! did-set #t)
	       (hset! h (list x y) n))
	      ((and (integer? ch) (< n ch)) ;; an integer here - 
	       (fmt #t "found better value for square ~a ~a" x y)
	       (set! did-set #t)
	       (hset! h (list x y) n))))))) ;; otherwise i dont care #\# wall or border of grid
	      
    
    (define (forward n)
      (let loop ((x 1)(y 1))
	(cond
	 ((> x width)	  
	  (loop 1 (+ y 1)))
	 ((> y height) #f)
	 (#t
	  (let ((ch (href h (list x y))))
	    (when (and (integer? ch) (= ch n))
	      (go (- x 1) y (+ n 1))
	      (go (+ x 1) y (+ n 1))
	      (go  x (- y 1) (+ n 1))
	      (go  x (+ y 1) (+ n 1)))	    
	    (loop (+ x 1) y))))
	(when did-set ;; use a flag to tell us if we set anything otherwise loop forever
	  (set! did-set #f)
	  (forward (+ n 1)))))

    ;; give start value of 0
    (hset! h start 0)
    ;; look for squares that are empty in all four directions , if so label then 1
    ;; loop again looking for squares labelled 1 and all four directions , label then 2
    ;; so on
    (forward 0)

    (let ((exit (href h end)))      
      exit)))


;;       
;; # # # # 
;; any two squares 1 2 is same as 2 1 since only one of the squares will be entered first by breadth first search
;; only need place 2nd cheat square either + x 1 or + y 1 only and this will cover all possible 
;; do both squares need to be activated ?
;; cheat : ensure hash has only empty squares or walls , start end squares erased to empty squares before here
(define (cheat h)
  (let ((start (href h 'start))
	(end (href h 'end))
	(width (href h 'width))
	(height (href h 'height))
	(cheat-hash (make-hash-table))
	(nominal (let ((h2 (copy-hash h)))
		   (let ((exit (wander! h2)))
		     (show-hash h2)
		     exit))))

    ;; (define (go x y x2 y2) ;; try put an integer at x y where there is an empty #\. char 
    ;;   (cond
    ;;    ((<= x 1) #f)
    ;;    ((<= y 1) #f)       
    ;;    ((>= x width) #f)
    ;;    ((>= y height) #f)
    ;;    ((<= x2 1) #f)
    ;;    ((<= y2 1) #f)       
    ;;    ((>= x2 width) #f)
    ;;    ((>= y2 height) #f)
    ;;    (#t (let ((copy (copy-hash h)))
    ;; 	     (hset! copy (list x y) #\.)
    ;; 	     (hset! copy (list x2 y2) #\.)
    ;; 	     (hset! copy 'cheat-1 (list x y))
    ;; 	     (hset! copy 'cheat-2 (list x2 y2))
    ;; 	     (let* ((exit (wander! copy))
    ;; 		    (savings (- nominal exit)))
    ;; 	       (when (= savings 64) ;;(> savings 0)
    ;; 		 (fmt #t "placing cheats at (~a ~a) (~a ~a) saved ~a picoseconds~%" x y x2 y2 savings)		 
    ;; 		 (show-hash copy)
    ;; 		 ;;(fmt #t "~%~%")
    ;; 		 ))))))


    (define (go2 x y) ;; try put an integer at x y where there is an empty #\. char 
      (cond
       ((<= x 1) #f)
       ((<= y 1) #f)       
       ((>= x width) #f)
       ((>= y height) #f)
       (#t (let ((copy (copy-hash h)))
	     (hset! copy (list x y) #\.)
	     (hset! copy 'cheat-1 (list x y))
	     (let* ((exit (wander! copy))
		    (savings (- nominal exit)))
	       (when (> savings 0) 
		 (fmt #t "placing cheat at (~a ~a) saved ~a picoseconds~%" x y savings)
		 (let ((val (href/d cheat-hash savings #f)))
		   (cond
		    ((and (list? val) (member (list x y) val)) #f)
		    ((list? val) (hset! cheat-hash savings (cons (list x y) val)))
		    ((eq? val #f) (hset! cheat-hash savings (list (list x y))))))
		 ;;(show-hash copy)
		 ;;(fmt #t "~%~%")
		 ))))))
    
       

    ;; loop 
    (let loop ((x 1)(y 1))
	(cond
	 ((> x width)	  
	  (loop 1 (+ y 1)))
	 ((> y height) #f)
	 (#t

	  ;; (go x y (- x 1) y)
	  ;; (go x y (+ x 1) y)
	  ;; (go x y x (- y 1))
	  ;; (go x y x (+ y 1))

	  (go2 (- x 1) y)
	  (go2 (+ x 1) y)
	  (go2 x (- y 1))
	  (go2 x (+ y 1))
	  (loop (+ x 1) y))))
    
    ;; go through the cheat hash what do i find
    (let ((keys (hash-table-keys cheat-hash))
	  (tot 0))
      (do-list (pico keys)
	       (let* ((val (href cheat-hash pico))
		      (ncheats (length val)))
		 (fmt #t "There are ~a cheats that save ~a picoseconds~%" ncheats pico)
		 ;; how many cheats save 100 pico seconds
		 (when (>= pico 100)
		   (set! tot (+ tot ncheats)))))
      
      (fmt #t "There are ~a cheats that save 100 picoseconds~%" tot))))




;; cheats placed next to each other , not two places at random on board 

(cheat hash)



    
	
  
  


  
  
 
 


















