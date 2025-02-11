

(load "../../macros/import.scm")
(load "../../macros/for.scm")
(load "../../macros/incf.scm")
(load "../../macros/grid.scm")

;; debug simply prints all its args 
(define (debug . args)
  (map (lambda (x) (format #t "~a " x)) args)
  (format #t "~%"))


(define (onboard? g x y)
  (cond
   ((< x 1) #f)
   ((> x (g 'wid)) #f)
   ((< y 1) #f)
   ((> y (g 'hgt)) #f)
   (#t #t)))
    

;; 1st entry to develop , assign r x y <- i
;; vec will hold area - ie number in crop , 2nd element
;; vec.1 = area
;; vec.2 = perimeter
(define (develop-region crop i r g x y)
  (let ((vec (make-vector 3 0))) ;; atleast 2 element long
    (r 'set! x y i)
    (vector-set! vec 1 1)

    ;; if below is off board or of a different crop - then increment perim count
    (when (or (not (onboard? g x (+ y 1)))
	      (let ((crop3 (g 'get x (+ y 1))))
		(not (equal? crop crop3))))
      (vector-set! vec 2 (+ 1 (vector-ref vec 2))))
    
    ;; if above is off board or of a different crop - then increment perim count
    (when (or (not (onboard? g x (- y 1)))
	      (let ((crop3 (g 'get x (- y 1))))
		(not (equal? crop crop3))))
      (vector-set! vec 2 (+ 1 (vector-ref vec 2))))
    
    ;; if left is  off board or of a different crop - then increment perim count
    (when (or (not (onboard? g (- x 1) y))
	      (let ((crop3 (g 'get (- x 1) y)))
		(not (equal? crop crop3))))
      (vector-set! vec 2 (+ 1 (vector-ref vec 2))))
    
    ;; if right is off board or of a different crop - then increment perim count
    (when (or (not (onboard? g (+ x 1) y))
	      (let ((crop3 (g 'get (+ x 1) y)))
		(not (equal? crop crop3))))
      (vector-set! vec 2 (+ 1 (vector-ref vec 2))))
    

    ;; recursively search for more regions but in recursive version check if square r x y is zero and r x y has same crop
    ;; as original square , if so assign i to that square and
    (recurse-region crop vec i r g (- x 1) y)
    (recurse-region crop vec i r g (+ x 1) y)
    (recurse-region crop vec i r g x (+ y 1))
    (recurse-region crop vec i r g x (- y 1))
    vec))



(define (recurse-region crop vec i r g x y)
  (cond
   ((not (onboard? r x y)) #f)
   (#t (let ((e (r 'get x y))
	     (crop2 (g 'get x y)))
	 (when (and (equal? crop crop2)
		    (zero? e))
	   (r 'set! x y i)	   
	   ;; add 1 to area count 
	   (vector-set! vec 1 (+ 1 (vector-ref vec 1))) 
	
	   ;; if below is off board or of a different crop - then increment perim count
	   (when (or (not (onboard? g x (+ y 1)))
		     (let ((crop3 (g 'get x (+ y 1))))
		       (not (equal? crop crop3))))
	     (vector-set! vec 2 (+ 1 (vector-ref vec 2))))

	   ;; if above is off board or of a different crop - then increment perim count
	   (when (or (not (onboard? g x (- y 1)))
		     (let ((crop3 (g 'get x (- y 1))))
		       (not (equal? crop crop3))))
	     (vector-set! vec 2 (+ 1 (vector-ref vec 2))))

	   ;; if left is  off board or of a different crop - then increment perim count
	   (when (or (not (onboard? g (- x 1) y))
		     (let ((crop3 (g 'get (- x 1) y)))
		       (not (equal? crop crop3))))
	     (vector-set! vec 2 (+ 1 (vector-ref vec 2))))

	   ;; if right is off board or of a different crop - then increment perim count
	   (when (or (not (onboard? g (+ x 1) y))
		     (let ((crop3 (g 'get (+ x 1) y)))
		       (not (equal? crop crop3))))
	     (vector-set! vec 2 (+ 1 (vector-ref vec 2))))

	   (recurse-region crop vec i r g (- x 1) y)
	   (recurse-region crop vec i r g (+ x 1) y)
	   (recurse-region crop vec i r g x (+ y 1))
	   (recurse-region crop vec i r g x (- y 1)))))))


;; regions
(define (find-regions g)
  (let ((results '())
	(r (g 'copy))
	(i 0)) ;; first region to be found gets id one 
    (for (x 1 (r 'wid))
	 (for (y 1 (r 'hgt))
	      (r 'set! x y 0)))
    (debug "cleared r ")
    (r 'show)
    
    ;; all squares set to 0 - no regions here
    ;; all other regions 1 to N for n regions
    (debug "looking for regions ...")
    (for (x 1 (r 'wid))
	 (for (y 1 (r 'hgt))
	      (let ((e (r 'get x y))
		    (crop (g 'get x y)))
		(debug "->" e "crop->" crop)
		(cond ;; if e is 0 then its a newly encountered region
		 ((zero? e)
		  (incf! i)
		  (let ((vec (develop-region crop i r g x y)))
		    (set! results (cons (list 'crop crop 'region i 'area (vector-ref vec 1) 'perim (vector-ref vec 2)) results))))))))
		  
    ;; all regions should be numbered , since no empty squares
    ;; there should be no zeros in the grid
    (debug "there are " i " regions ")
    (list results r)))

#|
vertically couple cases
    case 1 : 
               *x*       

                  x
    case 2 :     *x*             *x*
                                  x
                   x
    case 3 :      *x*
                   x

for crop on the border do they get a fence ?  yes

|#


(define (/= x y) (not (= x y)))

#|

;; requires we search entire area when we could just search a small region 
;; or if we knew where those squares were , we could just iterate over them and ask whatever side is either
;; off the board or has a different crop or different region

code in terms of positions 
         left P
         right P
         up P
         down P 


|# 
(define (perimeter g n)
  (let ((perim 0))
    (for (x 1 (r 'wid))
	 (for (y 1 (r 'hgt))
	      (let ((e (g 'get x y )))
		(when (= e n)
		  (let ((eup (if (not (onboard? g x (- y 1))) #f (g 'get x (- y 1))))
			(edown (if (not (onboard? g x (+ y 1))) #f (g 'get x (+ y 1))))
			(eleft (if (not (onboard? g (- x 1) y)) #f (g 'get (- x 1) y)))
			(eright (if (not (onboard? g (+ x 1) y)) #f (g 'get (+ x 1) y))))
		    (when (not(equal? e eup)) (incf! perim))
		    (when (not(equal? e edown)) (incf! perim))
		    (when (not(equal? e eleft)) (incf! perim))
		    (when (not(equal? e eright)) (incf! perim))
		    )))))
    perim))



(define (grid-from-list width height xs) 
  (let ((len (length xs))
	(len2 (* width height)))
    (assert (= len len2))
    (let ((g (make-grid width height 'X)))
      (for (y 1 height)
	   (for (x 1 width)
		(let ((e (car xs)))          
		  (g 'set! x y e)            
		  (set! xs (cdr xs)))))
      g)))


  
	
;; rg contains results . grid 
(define (price-regions rg)
  (let ((crop-regions (car rg)))
    (debug "crop-regions -> " crop-regions)

    ;; (apply + (map (lambda (x)
    ;; 		    (let ((area (list-ref x 6))
    ;; 			  (perimeter (list-ref x 8)))
    ;; 		      (* area perimeter)))
    ;; 		  (car rg)))
    ;;(let ((price (lambda (x) (* (list-ref x 6) (list-ref x 8)))))
    (apply + 
    (map (lambda (x) 
	   (let ((perimeter (list-ref x 7))
		 (area (list-ref x 5)))
	     (debug "crop -> perimeter :"  perimeter " area : "  area)
	     (* area perimeter))) 
	 crop-regions))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;; make-grid-from-list
;; (define g (let ((g (make-grid 4 4 'X)))
;; 	    (let ((xs '( A A A A
;; 			 B B C D
;; 			 B B C C
;; 			 E E E C)))
;; 	      (for (y 1 4)
;; 		(for (x 1 4)
;; 		  (let ((e (car xs)))          
;; 		    (g 'set! x y e)            
;; 		    (set! xs (cdr xs))))))                
;; 	    g))

(define g (grid-from-list 4 4 '( A A A A
				 B B C D
				 B B C C
				 E E E C)))

                                                         
(define rg (find-regions g))                                                                                                         
                                           
   
(define g2 (grid-from-list 5 5 '(
 O O O O O 
 O X O X O 
 O O O O O 
 O X O X O 
 O O O O O )))

(define rg2 (find-regions g2))

(define g3 (grid-from-list 10 10 '(
R R R R I I C C F F 
R R R R I I C C C F
V V R R R C C F F F 
 V V R C C C J F F F 
 V V V V C J J C F E 
 V V I V C C J J E E 
 V V I I I C J J E E 
 M I I I I I J J E E 
 M I I I S I J E E E 
 M M M I S S J E E E 
)))

(define rg3 (find-regions g3))
    

;; part 1 need to read all values from ../input.txt

(define input-filename "../input.txt")
(define lines (with-input-from-file input-filename
		(lambda ()
		  (read-lines))))
;; input is 140x140

;; string->list 
(define in (apply append (map string->list lines)))
(define gin (grid-from-list 140 140 in))
(define rin (find-regions gin))
(define part-1 (price-regions rin))

(debug "price regions for input is " part-1)
















