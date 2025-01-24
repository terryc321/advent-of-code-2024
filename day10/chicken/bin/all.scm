

;; grid.scm
;; test-grid.scm


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



(define *example2* ;; 7 x 7 
  (make-grid 7 7
  '(
 -9 -9 -9 0 -9 -9 -9 
 -9 -9 -9 1 -9 -9 -9 
 -9 -9 -9 2 -9 -9 -9 
 6 5 4 3 4 5 6 
 7 -9 -9 -9 -9 -9 7 
 8 -9 -9 -9 -9 -9 8 
 9 -9 -9 -9 -9 -9 9
)))


(define *example3*  ;;  8 x 8
  (make-grid 8 8
  '(
 8 9 0 1 0 1 2 3 
 7 8 1 2 1 8 7 4 
 8 7 4 3 0 9 6 5 
 9 6 5 4 9 8 7 4 
 4 5 6 7 8 9 0 3 
 3 2 0 1 9 0 1 2
 0 1 3 2 9 8 0 1 
 1 0 4 5 6 7 3 2 
)))

(define *example4* ;; 7 x 7
  (make-grid 7 7 
             '(
               20 20 20 0 20 20 20
               20 20 20 1 20 20 20
               20 20 20 2 20 20 20
               6 5 4 3 4 5 6
               7 20 20 20 20 20 7
               8 20 20 20 20 20 8
               9 20 20 20 20 20 9
                  )))



(define *example5* ;; 7 x 7
  (make-grid 7 7 
             '(
               20 20 9 0 20 20 9
                  20 20 20 1 20 9 8
                  20 20 20 2 20 20 7
                  6 5 4 3 4 5 6
                  7 6 5 20 9 8 7 
                  8 7 6 20 20 20 20
                  9 8 7 20 20 20 20
                  )))

(define *example6* ;; 7 x 7
  (make-grid 7 7 
             '(
                 1 0 20 20 9 20 20
                 2 20 20 20 8 20 20
                 3 20 20 20 7 20 20
                 4 5 6 7 6 5 4
                 20 20 20 8 20 20 3
                 20 20 20 9 20 20 2
                 20 20 20 20 20 0 1
                 )))




 
 (define *puzzle* ;; 53 x 53
   (make-grid 53 53
    '(
7 8 7 6 0 3 4 5 8 0 1 2 9 8 8 9 6 3 2 1 0 0 1 2 3 0 1 1 0 9 7 6 3 0 1 8 7 0 1 3 4 3 4 1 0 3 4 5 4 5 6 8 7
6 9 6 7 1 2 3 6 9 8 9 3 6 7 7 6 5 4 1 0 6 7 8 3 4 5 2 2 1 8 8 5 4 3 2 9 6 3 2 3 3 4 5 2 1 2 7 6 3 2 7 9 6
1 0 5 8 9 9 8 7 6 7 0 4 5 6 0 0 1 3 2 3 5 6 9 8 9 6 3 3 6 7 9 7 0 1 0 1 5 4 3 2 4 5 6 9 4 9 8 1 0 1 8 4 5
2 3 4 7 8 6 7 8 5 6 1 0 3 2 1 3 2 1 2 4 4 2 8 9 8 7 4 4 5 7 8 8 9 2 3 2 4 5 0 1 3 4 7 8 5 4 5 6 1 3 9 3 2
1 2 0 3 4 5 6 9 4 3 2 9 8 3 0 4 5 0 3 4 3 1 0 1 7 6 5 9 6 6 5 7 6 5 4 3 5 6 9 3 2 1 0 3 4 5 6 7 5 4 3 0 1
0 6 1 2 3 7 6 5 3 0 1 8 7 4 1 2 6 9 6 5 2 2 3 4 5 6 7 8 0 3 4 5 5 4 3 2 6 7 8 4 3 2 1 2 7 6 9 8 6 1 2 1 2
6 7 6 1 0 8 5 4 3 2 2 9 6 5 4 3 7 8 7 6 1 0 4 3 2 0 6 9 1 2 7 5 6 7 6 1 8 9 7 3 2 3 0 1 8 9 0 7 7 0 0 2 3
9 8 5 4 5 9 2 1 0 1 3 4 5 6 9 4 6 7 6 6 7 8 2 1 1 1 2 3 4 9 8 7 0 8 9 0 7 8 7 2 1 4 5 0 3 2 1 9 8 1 1 6 7
2 3 4 3 4 5 3 0 1 4 5 6 9 7 8 5 4 3 2 5 6 9 3 0 0 3 4 3 5 6 7 2 1 2 8 5 6 9 6 7 0 9 6 5 4 9 2 8 4 3 2 9 8
1 0 1 2 7 6 4 7 6 5 4 7 8 7 7 6 5 0 1 4 2 3 4 5 1 2 3 4 5 9 6 5 4 3 2 1 5 4 5 8 9 8 7 6 7 8 2 1 9 4 5 8 7
0 1 0 1 8 7 5 8 9 4 3 4 3 9 8 9 0 1 4 3 1 0 9 6 7 1 2 3 6 8 7 8 9 4 1 0 0 3 4 3 2 1 0 6 5 4 3 0 8 7 6 5 6
1 0 1 2 9 6 6 7 8 7 2 1 0 8 7 4 3 2 3 2 1 9 8 7 8 0 9 8 7 5 6 7 6 5 0 0 1 2 3 4 3 2 6 7 8 9 0 9 8 9 9 4 3
7 8 7 3 4 5 3 1 2 3 1 0 6 5 6 5 4 6 5 1 0 1 5 6 9 7 8 9 8 2 3 2 1 0 9 8 7 1 0 5 4 1 5 4 9 0 1 0 7 7 8 3 0
6 9 6 3 2 3 4 0 3 4 5 6 7 6 5 4 4 5 4 3 2 3 4 5 0 6 7 0 1 1 4 5 6 9 6 7 8 0 5 6 7 0 6 3 2 1 2 5 6 6 3 2 1
1 2 5 4 1 2 5 6 5 4 3 9 8 9 0 3 5 6 7 2 1 0 3 2 1 5 0 1 2 0 9 8 7 8 5 9 9 1 2 7 8 3 4 3 1 0 3 4 5 5 4 1 0
0 1 2 3 0 1 6 7 6 1 2 1 0 8 1 2 0 7 8 0 0 4 3 4 5 4 1 2 3 2 1 0 0 3 4 8 7 6 3 6 9 7 5 2 1 0 1 7 6 9 5 1 1
4 3 4 5 6 8 9 8 9 0 9 8 1 7 4 3 1 8 9 1 0 1 2 7 6 3 2 0 4 9 8 7 1 2 1 7 6 5 4 5 9 8 6 1 0 1 9 8 7 8 7 0 0
3 2 1 9 7 8 9 9 7 6 8 7 4 6 5 3 2 9 8 2 3 6 7 8 7 4 2 1 5 6 7 2 1 0 0 8 9 4 3 1 2 7 7 2 9 2 3 4 6 7 0 1 1
4 4 0 8 9 4 3 2 8 9 7 6 5 5 6 7 3 6 7 5 4 9 8 9 8 5 3 6 5 5 2 3 4 5 6 7 8 3 2 0 3 6 8 9 8 7 6 5 5 4 3 2 2
4 3 4 7 6 5 2 1 7 8 4 5 5 4 9 8 4 5 6 6 7 8 0 1 8 9 4 7 4 3 1 0 3 2 7 8 9 4 1 0 4 5 0 1 2 9 8 4 5 8 9 4 3
3 2 5 6 2 3 4 0 6 0 3 4 4 3 2 9 5 4 3 2 1 1 0 2 7 6 5 8 9 2 1 0 0 1 6 7 6 5 0 6 7 8 9 8 3 4 7 4 5 7 6 5 4
4 1 0 0 1 4 5 6 5 1 2 7 8 2 1 0 6 7 9 8 0 2 3 3 4 5 3 4 1 2 3 2 1 0 8 7 7 8 7 5 4 1 0 6 7 5 6 3 4 6 6 5 5
5 8 9 8 9 8 6 7 4 2 5 6 9 1 0 6 5 8 9 7 6 5 4 3 0 1 2 5 0 3 4 3 4 1 9 6 8 9 6 0 3 2 4 5 8 9 1 2 5 8 7 4 3
6 7 6 7 0 1 9 8 3 0 4 7 8 1 4 7 8 6 5 4 7 6 1 2 9 8 7 6 5 4 3 0 5 8 9 5 7 6 5 1 2 2 3 1 2 3 0 9 6 9 5 1 2
5 0 5 0 1 0 7 8 2 1 3 2 1 0 5 6 9 7 6 3 8 9 0 1 8 1 0 5 6 5 2 1 6 7 7 4 3 3 4 4 3 1 0 0 9 4 5 8 7 2 4 0 2
4 1 4 3 2 1 8 9 3 6 2 3 8 7 6 5 9 8 1 2 0 8 3 2 1 2 3 4 7 8 9 3 4 7 8 9 2 2 1 0 9 8 1 1 8 7 6 7 2 1 3 2 1
3 2 3 4 3 2 7 6 6 5 1 0 9 6 5 4 3 2 0 2 1 4 4 3 0 7 4 3 8 9 8 2 5 6 0 0 1 3 0 7 8 3 2 0 1 2 5 6 3 0 4 3 0
2 1 0 5 4 5 6 5 7 8 9 8 2 3 4 3 0 1 4 3 4 3 5 6 9 8 7 2 9 8 6 1 0 1 1 2 2 4 5 6 5 4 1 0 4 3 2 1 4 5 5 6 1
7 8 7 6 9 8 3 4 6 5 8 7 1 2 8 9 4 3 5 4 9 9 8 7 0 5 6 1 0 1 5 4 3 9 8 3 4 8 9 0 1 5 6 5 5 8 9 0 0 6 7 7 6
6 9 8 7 6 7 6 5 5 4 1 0 0 6 7 6 5 2 6 7 8 8 8 2 1 4 5 6 7 8 6 3 2 8 7 6 5 7 6 5 2 6 5 1 6 7 1 2 1 7 8 8 9
5 6 9 4 5 4 5 4 5 3 2 2 1 4 8 9 0 1 2 2 3 7 9 8 2 3 0 9 8 9 7 8 1 0 1 0 9 8 3 4 3 7 8 0 9 8 0 3 6 8 9 2 3
4 7 8 3 4 5 0 3 6 5 4 3 2 3 0 1 2 0 0 1 4 5 6 7 0 2 1 1 2 1 0 9 7 8 0 1 2 3 2 1 0 8 9 1 2 7 6 4 5 6 9 1 4
3 2 3 2 2 1 1 2 5 6 7 4 1 0 1 6 3 1 0 0 0 1 4 5 1 1 0 0 3 4 9 8 8 9 8 1 9 4 5 4 1 2 0 1 3 4 5 5 6 7 8 0 5
0 1 0 1 2 0 1 2 3 9 8 7 8 9 6 5 4 0 1 1 7 2 3 6 2 1 0 1 4 5 8 6 7 6 7 0 8 7 6 8 7 6 3 2 4 3 6 6 7 9 8 7 6
0 1 2 0 1 0 3 4 4 9 8 2 1 8 7 6 5 3 2 7 8 9 8 7 3 4 7 6 5 6 7 5 4 3 2 1 0 1 5 9 8 5 4 3 5 6 7 9 8 7 0 1 5
3 2 7 8 9 1 2 5 5 6 7 3 0 9 0 1 4 4 5 6 7 8 3 3 4 9 8 9 8 9 8 7 0 1 3 2 1 6 9 6 7 8 4 5 6 7 8 8 7 2 1 2 6
4 5 6 9 4 8 7 6 3 2 3 4 1 8 1 2 3 2 4 2 1 2 4 4 5 6 7 8 7 0 1 6 5 2 4 5 6 7 8 5 6 9 3 4 5 6 9 5 4 3 0 3 4
3 4 5 4 3 9 4 5 4 1 0 5 6 7 0 3 2 1 0 3 0 2 3 4 5 6 6 8 9 1 4 5 4 3 4 7 8 7 6 4 3 0 2 1 0 7 8 6 7 6 5 4 5
6 7 4 3 2 8 7 6 5 0 9 8 7 8 7 4 9 8 5 4 5 1 0 9 6 5 6 7 0 2 3 6 5 4 3 6 9 4 5 3 2 1 2 2 1 8 9 0 8 9 4 9 6
7 8 7 6 1 7 8 9 0 3 4 5 6 9 8 5 8 9 8 7 6 2 7 8 7 4 3 2 1 9 8 7 2 3 1 2 4 3 2 3 1 8 2 1 6 7 2 1 0 2 3 8 7
8 9 4 5 0 3 2 1 1 2 3 9 8 8 5 6 7 5 4 6 5 9 6 7 6 5 3 4 5 6 9 8 1 0 0 3 4 0 1 0 0 9 1 0 5 6 5 2 0 1 2 5 6
4 4 3 2 1 4 5 0 2 1 0 6 7 6 8 7 5 6 5 4 5 8 7 0 3 4 2 7 6 7 6 5 6 5 7 6 5 4 3 2 7 8 2 3 4 3 4 3 8 9 8 5 5
3 2 3 4 0 5 6 5 0 1 2 3 6 3 9 6 6 7 0 3 4 5 6 1 2 3 1 8 9 8 9 4 3 9 8 7 8 9 0 1 6 4 3 4 3 2 5 6 7 0 7 6 5
0 1 0 5 1 8 7 6 7 6 5 4 5 4 5 6 7 8 1 2 8 9 0 0 1 2 0 0 7 6 5 4 2 8 9 4 3 0 1 4 5 0 4 5 4 1 0 5 8 1 6 7 8
9 8 7 6 2 9 9 8 8 9 8 3 2 3 2 3 4 9 0 6 7 2 1 0 6 3 2 1 8 7 2 3 1 7 6 5 2 1 0 3 2 1 5 6 9 2 1 8 9 2 5 6 9
4 5 1 0 1 2 3 7 0 1 2 2 1 0 1 0 5 4 1 5 4 3 0 8 7 4 3 4 9 8 1 2 0 6 7 8 9 8 1 4 9 8 6 7 8 6 5 4 3 3 4 5 4
3 6 7 8 8 9 4 6 5 4 3 1 0 1 0 7 6 0 1 0 9 8 7 9 6 5 4 5 6 7 0 1 0 9 8 7 7 8 9 2 8 7 6 6 4 5 6 5 2 1 0 4 3
2 9 8 9 7 6 5 4 2 1 8 7 6 0 1 8 7 6 2 3 8 7 5 4 7 8 3 0 9 8 9 8 1 0 5 6 6 7 8 1 0 6 1 0 3 4 7 8 7 6 5 4 2
1 0 8 9 8 7 4 3 0 0 9 4 5 1 2 9 6 8 7 4 5 6 6 3 2 9 2 1 4 3 2 1 2 3 4 3 5 4 3 2 0 5 2 1 2 9 8 9 8 5 0 3 1
0 1 7 3 4 5 4 2 1 1 2 3 2 1 3 4 5 9 8 7 0 1 7 8 1 2 1 0 5 6 7 0 9 8 5 0 1 2 1 3 1 4 9 4 5 6 7 8 7 4 1 2 0
4 5 6 2 9 6 1 0 3 2 1 0 3 0 9 9 6 5 0 6 3 2 3 9 0 1 0 7 8 9 8 7 8 7 6 5 4 3 0 1 2 3 8 3 4 5 6 9 2 3 4 5 4
3 0 0 1 8 7 2 3 4 7 8 9 4 5 8 8 7 2 1 5 4 8 9 7 6 5 2 1 2 9 8 5 4 3 2 6 9 2 1 0 5 6 7 2 3 2 1 0 1 2 7 6 5
2 1 1 6 7 6 3 4 5 6 9 8 7 6 7 7 4 3 4 5 6 7 7 8 9 4 3 0 3 8 7 6 5 0 1 7 8 3 2 3 4 5 6 1 0 3 0 1 0 9 8 7 6
     )
)
)


;; ================================ test-grid.scm ===================================

;; https://wiki.call-cc.org/eggref/5/test#test

(import test)

;; how does one know how to import grid ?
;; (import grid)

;; ;; Simple test
;; (test 4 (+ 2 2))
;; 
;; ;; group
;; (test-group "A group"
;;   (test "A test with description" 5 (+ 2 3))
;;   (test-assert "This should always be true" (string? "foo")))
;; 
;; ;; group
;; (test-group "B group"
;;   (test "division by zero" 5 (/ 1 0)))


;; how do we do test suite in chicken scheme ?
;; a local test 
;; if something goes wrong in test then 
((lambda () 
   (letrec ((g   (make-grid 4 4
                            '(0 1 2 3
                                1 2 3 4 
                                8 7 6 5
                                9 8 7 6))))
     (let* ((h (g 'copy)))
       (test-group "copying"
         (test-assert "original untouched"
           (equal? (h 'arr) #( 0 1 2 3
                               1 2 3 4 
                               8 7 6 5
                               9 8 7 6))))))))




((lambda () 
   (letrec ((g   (make-grid 4 4
                            '(0 1 2 3
                                1 2 3 4 
                                8 7 6 5
                                9 8 7 6))))
     (let* ((h (g 'copy)))
       (h 'set 0 0 9)
       (test-group "simple mutation holds"
         (test-assert "original unchanged" 
           (equal? (g 'arr) #( 0 1 2 3
                               1 2 3 4 
                               8 7 6 5
                               9 8 7 6)))
         (test-assert "mutated square 0 0 to 9"
           (equal? (h 'arr) #( 9 1 2 3
                               1 2 3 4 
                               8 7 6 5
                               9 8 7 6))))))))

((lambda ()
   (letrec ((g   (make-grid 4 4
                            '(0 1 2 3
                                1 2 3 4 
                                8 7 6 5
                                9 8 7 6))))
     (test-group "test-3c"
       (test-assert "g width is 4" (= (g 'wid) 4))))))


((lambda ()
   (letrec ((g   (make-grid 4 4
                            '(0 1 2 3
                                1 2 3 4 
                                8 7 6 5
                                9 8 7 6))))
     
     (test-group "test-3b"
       (test-assert "g height is 4"  (= (g 'hgt) 4))))))


((lambda ()
   (letrec ((g   (make-grid 4 4
                            '(0 1 2 3
                                1 2 3 4 
                                8 7 6 5
                                9 8 7 6))))
     (test-group "test-3c"
       (test-assert "g 0 0 = 0" (= (g 'get 0 0) 0))))))


    


((lambda () 
    (letrec ((g   (make-grid 4 4
                             '(0 1 2 3
                               1 2 3 4 
                               8 7 6 5
                               9 8 7 6))))
      (test-group "test-3"
      (test-assert "width is 4" (= (g 'wid) 4))
      (test-assert "height is 4" (= (g 'hgt) 4))

      (test-assert "g 0 0 = 0" (= (g 'get 0 0) 0))
      (test-assert "g 1 0 = 1" (= (g 'get 1 0) 1))
      (test-assert "g 2 0 = 2" (= (g 'get 2 0) 2))
      (test-assert "g 3 0 = 3" (= (g 'get 3 0) 3))

      (test-assert "g 0 1 = 1" (= (g 'get 0 1) 1))
      (test-assert "g 1 1 = 2" (= (g 'get 1 1) 2))
      (test-assert "g 2 1 = 3" (= (g 'get 2 1) 3))
      (test-assert "g 3 1 = 4" (= (g 'get 3 1) 4))

      (test-assert "g 0 2 = 8" (= (g 'get 0 2) 8))
      (test-assert "g 1 2 = 7" (= (g 'get 1 2) 7))
      (test-assert "g 2 2 = 6" (= (g 'get 2 2) 6))
      (test-assert "g 3 2 = 5" (= (g 'get 3 2) 5))

      (test-assert "g 0 3 = 9" (= (g 'get 0 3) 9))
      (test-assert "g 1 3 = 8" (= (g 'get 1 3) 8))
      (test-assert "g 2 3 = 7" (= (g 'get 2 3) 7))
      (test-assert "g 3 3 = 6" (= (g 'get 3 3) 6))
      ))))




;; =========================== part-1 ============================

;; part-1.scm
;; (load "grid.scm")

;; *example1*

;; given a grid , can we solve it ?
;; tot total number of 9's we were able to reach 
;; at x y if square is a ZERO then its a starting square
;;
;; keep a seperate grid for where we can reach , initially all these squares are #f false
;; meaning we cannot reach them 
;; if it is true , we can reach them 
;; starting square is #t true 
;; so we begin with a blank grid 
;;  place an X at x,y
;;  then we ask can we
;;
;;

(define solver
  (lambda (g)
    (let ((tot 0)          
          (wid-1 (- (g 'wid) 1))
          (hgt-1 (- (g 'hgt) 1)))
      (iter 0 hgt-1
            (lambda (y)
              (iter 0 wid-1
                    (lambda (x)
                      ;;(fmt #t "x = ~a y = ~a ~%" x y)
                      (let ((n (g 'get x y)))
                        (cond
                         ((zero? n)  ;; start search proper
                          ;; do not need another blanked grid
                          ;; (let ((v 'blank #f))
                          ;;   (v 'set x y #t)
                          ;; LOCAL list of nines found PER ZERO square , 
                          ;; rather than GLOBAL how many nines can we reach ...
                          (let ((found '()))
                            (searcher g x y 
                                      (lambda (x2 y2) 
                                        (cond
                                         ((member (list x2 y2) found) #f) ;; already counted
                                         (#t
                                          (fmt #t "reach at nine at x = ~a y = ~a ~%" x2 y2)
                                          (set! found (cons (list x2 y2) found))
                                          (set! tot (+ tot 1))))))))))))))
      tot)))




;; g grid 
;; v blanked grid with all #f on them ?? needed ?? 
;; x y position on grid x=0 .. wid-1 , y=0 .. hgt-1 inclusive 
;; z height if z = 9 then call fn 
;; 
(define searcher 
  (lambda (g x y fn)
    (let ((wid (g 'wid))
          (hgt (g 'hgt)))
      (letrec ((on-map? (lambda (x y) 
                          (and (>= x 0)(>= y 0)(< x wid)(< y hgt))))
               (move (lambda (x y z)
                     (cond
                      ((on-map? x y) 
                       (let ((z2 (g 'get x y)))
                         (cond
                          ((= (+ z 1) z2) ;; valid step
                           (cond  ;; if its a 9 we are done
                            ((= z2 9) (fn x y))                           
                            (#t   ;; otherwise took a valid step , move around
                             (move (+ x 1) y z2)
                             (move (- x 1) y z2)
                             (move x (+ y 1) z2)
                             (move x (- y 1) z2)
                             ))))))))))
      ;; give move a dummy -1 value so it will find a zero there ,
      ;; accept valid step 
      ;; look in four directions as required 
        (move x y -1)))))


;; =========================== run.scm ============================

(let ((sol (solver *puzzle*)))
  (fmt #t "the solution to part 1 is ~a nines found ~%" sol))


