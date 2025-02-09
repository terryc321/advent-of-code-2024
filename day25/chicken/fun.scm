


(import (chicken format))
(import (chicken pretty-print))
(import (srfi-1))
(import (simple-loops))
(define fmt format)
(import procedural-macros)
(import (chicken bitwise))
(import expand-full)

(import (chicken io))

(define ppe (lambda (x) (pp (expand* x))))

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
;; (define input-filename "../example.txt")


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

(grid 1)
(grid-rows (grid 1))
grid-count
(grid grid-count) ;;  ("#####" "#####" "#####" "#.#.#" "#.#.." "..#.." ".....")

(define (key? g)
  (let ((first-row (grid-row g 1))
	(last-row (grid-row g (grid-rows g))))
    (and (equal? first-row ".....")
	 (equal? last-row  "#####"))))



(define (lock? g)
  (let ((first-row (grid-row g 1))
	(last-row (grid-row g (grid-rows g))))
    (and (equal? first-row "#####")
	 (equal? last-row  "....."))))


;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; can open file read it , get grids , access grids , now what
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; locks have top row filled "#####" and bottom row empty "....."
;; keys have top row empty 

;; %%%% logic assertion that a grid must be either a KEY or a LOCK 

;; %%% logic lock does not have bits missing when counting down from top

;; (define (lock-pins g)
;;   (let ((wid (string-length (grid-row g 1)))
;; 	(hgt (length g)))
;;     (do-for (x 1 (+ wid 1))
;; 	    (do-for (ny 1 (+ hgt 1))
;; 		    (let ((y (- (+ hgt 1) ny)))
;; 		      (fmt #t "x = ~a : y = ~a ~%" x y))))))

(define (lock-pins g)
  (let ((wid (string-length (grid-row g 1)))
	(hgt (length g))
	(pins '()))
    ;; g must be a lock
    (assert (lock? g))
    (do-for (x 1 (+ wid 1))
	    (let ((pin (call/cc (lambda (done)
				  (do-for (y 1 (+ hgt 1))
					  (when (char=? (grid-xy g x y) #\.)
					    (done y)))
				  y))))
	      ;;(fmt #t "x = ~a : pin = ~a ~%" x (- pin 2))
	      (set! pins (cons (- pin 2) pins))))
    (reverse pins)))



(define (key-pins g)
  (let ((wid (string-length (grid-row g 1)))
	(hgt (length g))
	(pins '()))   
    ;; g must be a key
    (assert (key? g))
    (do-for (x 1 (+ wid 1))
	    (let ((pin (call/cc (lambda (done)
				  (do-for (y 1 (+ hgt 1))
					  (when (char=? (grid-xy g x (- hgt y)) #\.)
					    (done y)))
				  y))))
	      ;;(fmt #t "x = ~a : pin = ~a ~%" x (- pin 1))
	      (set! pins (cons (- pin 1) pins))))
    (reverse pins)))


;; 
(lock-pins (grid 1)) ;;  (0 5 3 4 3)
(key-pins (grid 3)) ;;  (5 0 2 1 3)



;; height of lock is 7 elements
;; last column in example lock-pin 3 key-pin 3
;; really should be lock pin has 4 elements , key pin has 4 elements total to 8 elements
;; but lock only holds 7 elements

(define (fits? g g2)
  (cond
   ((and (key? g) (lock? g2)) (fits-key-lock g g2))
   ((and (lock? g) (key? g2)) (fits-key-lock g2 g))
   (#t #f)))

(define (fits-key-lock key lock)
  (let ((hgt-key (length key))
	(hgt-lock (length lock)))
    ;; assume height of key should be same as lock
    (assert (= hgt-key hgt-lock))
    (let* ((sums (map (lambda (x y)(+ x y 2))
		      (key-pins key)
		      (lock-pins lock)))
	   (over (filter (lambda (x)(> x hgt-key)) sums)))
      (null? over))))

(define the-locks (filter lock? list-grids))

(define the-keys  (filter key? list-grids))

(define-macro (incf! x)
  `(set! ,x (+ ,x 1)))

(let ((a 1))
  (incf! a)
  a)


;; every key with every lock
(define (every-key-lock)
  (let ((fit 0))
    (do-list (key the-keys)
	     (do-list (lock the-locks)
		      ;; (fmt #t "%%%%% key lock %%%%%%  ~%")			      
		      ;; (pp key)
		      ;; (pp lock)
		      ;; (fmt #t "%%%%% key lock %%%%%%  ~%")			      
		      
		      (assert (key? key))
		      (assert (lock? lock))
		      
		      (let ((does (fits? key lock)))
			;; (fmt #t "key : ~a  with lock ~a : ~a ~%" key lock does)	
			(when does
			  (incf! fit)))))
    fit))


;; (every-key-lock) => 3439








  














