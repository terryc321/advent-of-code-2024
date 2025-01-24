

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
         ;; iteration macro goes from n to lim inclusive applies function fn to n
         (iter (lambda (n lim fn)
                 (cond
                  ((> n lim) #f)
                  (#t (fn n)
                      (iter (+ n 1) lim fn)))))
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
        ((eq? cmd 'getxy) (apply get-xy args))
        ((eq? cmd 'setxy) (apply set-xy args))
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

;; a local test 
;; if something goes wrong in test then 
(define test-1 
  (lambda () 
    (letrec ((g   (make-grid 4 4
                             '(0 1 2 3
                                 1 2 3 4 
                                 8 7 6 5
                                 9 8 7 6))))
      (g 'show)
      (let* ((h (g 'copy)))
        (h 'show)
        (equal? (h 'arr) #( 0 1 2 3
                                 1 2 3 4 
                                 8 7 6 5
                                 9 8 7 6))))))


(define test-2
  (lambda () 
    (letrec ((g   (make-grid 4 4
                             '(0 1 2 3
                                 1 2 3 4 
                                 8 7 6 5
                                 9 8 7 6))))
      (let* ((h (g 'copy)))
        (h 'setxy 0 0 9)
        (h 'show)
        (and
         (equal? (g 'arr) #( 0 1 2 3
                             1 2 3 4 
                             8 7 6 5
                             9 8 7 6))
         (equal? (h 'arr) #( 9 1 2 3
                             1 2 3 4 
                             8 7 6 5
                             9 8 7 6)))))))


        




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

