
;; some test cases 
;; (test-group "left tests"
;;   (test-assert "left ab" (equal? (left-half-str "ab") "a"))
;;   (test-assert "left abcd" (equal? (left-half-str "abcd") "ab"))
;;   (test-assert "left abcdef" (equal? (left-half-str "abcdef") "abc"))
;;   (test-assert "left abcdefgh" (equal? (left-half-str "abcdefgh") "abcd")))

;; this should print the length of the solution 
;; (fmt #t "solution ~a ~%" (length (repeat 25 blink puzzle)))

;; the 2nd solution gets hopelessly lost on 75 blinks ... somewhere around 40 blinks ... combinatorial explosion
;; this should print the length of the solution 
;; (fmt #t "solution ~a ~%" (length (repeat 75 blink puzzle)))




(import (chicken format))
(import test)
(define fmt format)
(import (chicken pretty-print))

(import (srfi-69))

;; load the grid abstractions 
;; load grid.scm 
;; load grid-test.scm


;; (iter 0 10 (lambda (n) (fmt #t "n = ~a ~%" n)))
;; some idea of connected-ness 
;; if start at any square that is in region A no matter which direction I go in what order I can
;; find all the attached squares forming this 

;; internal regions are simply finding regions that are not part of outside region , 
;; once that region has been found , it can be eliminated 
 
;; par edit mode
;; (find-region p1 0 0) -> '((0 0)(1 0)(2 0)(3 0)) 
;; simple reachable algorithm 
;;  A get an A at x y 
;;  is there an A at (- x 1) y ?
;;  is there an A at (+ x 1) y ?
;;  is there an A at x (+ y 1) ?
;;  is there an A at x (- y 1) ?


;; do not re-search the initial search square
(define find-region
  (lambda (g x y)
    (let ((wid (g 'wid))
          (hgt (g 'hgt)))                
      (letrec ((on-map?
                (lambda (x y)
                  (let ((result (and (>= x 0) (>= y 0) (< x wid) (< y hgt))))
                    (cond 
                     (result (fmt #t "~a ~a is on map . " x y ) #t)
                     (#t (fmt #t "~a ~a is not on map . " x y ) #f))))))
      
      (cond
       ((not (on-map? x y)) 
        (fmt #t "initial find-region ~a ~a is not on the map. " x y)
        #f)
       (#t 
        (let ((sym (g 'get x y))) ;; sym is in scope before define routines 
          (letrec ((seen-before?  
                    (lambda (pos seen)
                      (let ((result (member pos seen)))
                        (cond
                         (result (fmt #t "have seen before : ~a ~a " pos seen)
                                 result)
                         (#t (fmt #t "have not seen before : ~a ~a " pos seen)
                             #f)))))
                   
                   (searcher 
                    (lambda (g x y fn)
                      (fmt #t "calling searcher2 ...")                    
                      (searcher2 g x y '() fn)))
                   (searcher2
                    (lambda (g x y seen found-fn)
                      (cond
                       ((not (on-map? x y)) #f)
                       ((seen-before? (list x y) seen) #f)                     
                       (#t 
                        (let ((there (g 'get x y)))
                          (fmt #t "found symbol ~a..." there)                    
                          (cond
                           ((not (eq? there sym))
                            (fmt #t "does not match symbol ~a..." sym))
                           (#t
                            (fmt #t "does match symbol ~a..." sym)
                            (found-fn sym x y)
                            ;; search in all directions 
                            (searcher2 g (- x 1) y (cons (list x y) seen) found-fn)
                            (searcher2 g (+ x 1) y (cons (list x y) seen) found-fn)
                            (searcher2 g x (- y 1) (cons (list x y) seen) found-fn)
                            (searcher2 g x (+ y 1) (cons (list x y) seen) found-fn)))))))))
            ;; sym looking for is defined on entry to routine
            ;; x y is on map initial starting point
            (let* ((plot '())
                   (fn (lambda (sym x y)
                         (cond
                          ((and (on-map? x y) (not (seen-before? (list sym x y) plot)))
                           (set! plot (cons (list sym x y) plot)))))))
              (fmt #t "calling searcher ...")
              (searcher g x y fn)
              plot)))))))))


;; 
(define p1 
  (make-grid 4 4
   '( A A A A
      B B C D
      B B C C
      E E E C)))

;; 0 0 -> A region
;; 0 1 -> B region 
;; 0 3 -> E region 
;; 3 3 -> C region 
;; 3 1 -> D region 


(define p2 
  (make-grid 5 5
             '(  O O O O O
                 O X O X O
                 O O O O O
                 O X O X O
                 O O O O O)))

(define p3 
  (make-grid 10 10
             '(R R R R I I C C F F
                 R R R R I I C C C F
                 V V R R R C C F F F
                 V V R C C C J F F F 
                 V V V V C J J C F E 
                 V V I V C C J J E E 
                 V V I I I C J J E E
                 M I I I I I J J E E 
                 M I I I S I J E E E
                 M M M I S S J E E E)))




(define p1-regions 
  (list (find-region p1 0 0)
        (find-region p1 0 1)
        (find-region p1 0 3)
        (find-region p1 3 3)
        (find-region p1 3 1)))

(define iter-list 
  (lambda (xs fn)
    (cond
     ((null? xs) #f)
     (#t (let ((elem (car xs)))
           (fn elem)
           (iter-list (cdr xs) fn))))))


(define find-all-regions 
  (lambda (g)
    (let ((wid-1 (- (g 'wid) 1))
          (hgt-1 (- (g 'hgt) 1))
          (region-id 0)
          (region-hash (make-hash-table)))
      (iter 0 hgt-1 (lambda (y)
                      (iter 0 wid-1 (lambda (x) ;; does this x y position have a key in the hash ?
                                      (let ((plot-id (hash-table-ref/default region-hash (list x y) #f)))
                                        (cond 
                                         (plot-id #f) ;; already processed
                                         (#t 
                                          (let ((plot (find-region g x y)))
                                            (set! region-id (+ 1 region-id))
                                            (hash-table-set! region-hash region-id plot)
                                            (iter-list plot 
                                                       (lambda (cxy)
                                                         (let ((xy (cdr cxy)))
                                                           (hash-table-set! region-hash xy region-id))))
                                            ;; 
                                            (fmt #t "~% found region ~a ~%" plot)
                                            ;;
                                            ))))))))
      (values region-hash region-id))))

                                  
                                  
;; (define h (make-hash-table))
;; (hash-table-ref/default h 0 'nope)      
;; (hash-table-set! h 0 'yes)

(define (test)
  (iter-list '(1 2 3 4 5) (lambda (n)(fmt #t "n =~a~%" n))))


(define (test-p1)
  (find-all-regions p1))

(define (test-p2)
  (find-all-regions p2))

(define (test-p3)
  (call-with-values (lambda () (find-all-regions p3))
    (lambda (hash totid)
      (fmt #t "totid = ~a~%~%" totid)
      (iter 0 totid (lambda (id)
                      (fmt #t "region ~a : ~a ~%" id (hash-table-ref/default hash id #f)))))))



;; coming 






          


