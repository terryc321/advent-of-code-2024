

;; make me a grid abstraction from 
(import (chicken format))

(define fmt format)

;; grid abstraction ?
;; lack of tail call optimisation makes some routines less enjoyable to code


;; 2d grid bleed reachable algorithm 
;; find all the zeros 0 on the grid
;;  for each of those - copy grid -> then apply bleed algorithm , find all the X's you can
;;       we place an X over a 9 if that square is reachable

;; make a grid from a list of numbers 
;; this way a fresh vector will be created each time 


 ;; iteration macro goes from n to lim inclusive applies function fn to n
(define iter (lambda (n lim fn)
               (cond
                ((> n lim) #f)
                (#t (fn n)
                    (iter (+ n 1) lim fn)))))

(define make-grid 
  (lambda (wid hgt vals)
    (let* ((size (length vals))
           (arr (make-vector size)))
      ;; size vector is equal width * height
    (assert (= size (* wid hgt)))
    ;; size > 0 
    (assert (> size 0))
    ;; deep copy ... no loop macro ... from 0 to size-1 ...
    (letrec 
        (
         ;; copy vector contents across ...  
         ;; will not deep copy objects inside vector itself 
         ;; so vector of numbers will copy ok and mutating that will not affect other grids 
         ;; if grid has hash tables for example , that WILL be visible mutation outside initial grid !
         ;; so un-intended side effects ... documenting code ... 
         (initialize (lambda () 
                       (let ((arr2 (list->vector vals)))
                         (iter 0 (- size 1) (lambda (i)
                                              (vector-set! arr i (vector-ref arr2 i)))))))
         ;; grid access formula for zero based x = 0 y = 0 indices 
         (xy (lambda (x y) (+ x (* y wid))))
         ;; if we want to recreate another grid exact dimensions and contents 
         ;; fails 
         
         ;; copy-grid -> derive a copy of this grid
         (copy-grid (lambda () 
           (make-grid wid hgt vals)))

         ;; isnt this show grid ? ... smalltalk version of array
         (spew-grid 
          (lambda ()            
           (iter 0 (- hgt 1) 
                 (lambda (y) 
                   (fmt #t "~%")
                   (iter 0 (- wid 1) 
                         (lambda (x) 
               (fmt #t "self x: ~a y: ~a val: ~a . ~% " (+ x 1) (+ y 1) (vector-ref arr (xy x y)))))))))

           ;; add neighbours ??
           ;; (loop for y from 0 to (- hgt 1) do
           ;;   (loop for x from 0 to (- wid 1) do
           ;;     (when (on-board (- x 1) y)
           ;;     (format t "self x: ~a y: ~a str: '~a' ~% " x y (aref arr (xy x y)))))
           ;; )

         ;; we could just print each row , iterate along vector without need to XY compute since
         ;; next item is just one more than previous as ROW MAJOR matrix ...
         (show-grid 
          (lambda ()
            (fmt #t "~%")
            (iter 0 (- hgt 1) (lambda (y) 
                                (fmt #t "~%")
                                (iter 0 (- wid 1) (lambda (x) 
                                                    (fmt #t "~a " (vector-ref arr (xy x y)))))))))
         ;; arr may not be a valid vector ?
         ;; x , y may be out of bounds on rectangular coordinates expected 
         (get-xy (lambda (x y)
                   (vector-ref arr (xy x y))))

         ;; x y maybe invalid indices , may be 1 based not zero based as expected 
         ;; no checking , indeed how would you check+ report error ?
         (set-xy (lambda (x y z)
                   (vector-set! arr (xy x y) z)))

         );; letrec
      ;; initialize arr vector
        (initialize)
        ;; return object that can do stuff
    (lambda (cmd . args)
      (cond
        ((eq? cmd 'size) size)
        ((eq? cmd 'wid) wid)
        ((eq? cmd 'hgt) hgt)
        ((eq? cmd 'arr) arr)
        ((eq? cmd 'show) (show-grid))
        ((eq? cmd 'get) (apply get-xy args))
        ((eq? cmd 'set) (apply set-xy args))
        ((eq? cmd 'copy) (copy-grid))
        ((eq? cmd 'spew) (spew-grid))
        (#t (error "message not understood"))
        ))))))




;; how do we test the grid abstraction ,
;; where does it go wrong ?
;; pitfalls / performance issues 
;; what about a truely massive matrix multiplication how would that be best coded ?

;; #(0 1 2 3) is not a fresh vector - static one , so if it gets mutated it will be 
;; 


(define *example1* ;; 4 x 4 
  (make-grid 4 4
  '(0 1 2 3
    1 2 3 4 
    8 7 6 5
    9 8 7 6)))



