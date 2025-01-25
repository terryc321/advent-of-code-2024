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

(test-group "Grid Tests"
;; test 1
((lambda () 
    (letrec ((g   (make-grid 4 4
                             '(0 1 2 3
                                 1 2 3 4 
                                 8 7 6 5
                                 9 8 7 6))))
      (let* ((h (g 'copy)))
        (test-assert "copied array same contents"
          (equal? (h 'arr) #( 0 1 2 3
                              1 2 3 4 
                              8 7 6 5
                              9 8 7 6)))))))
;; test2
((lambda () 
   (letrec ((g   (make-grid 4 4
                            '(0 1 2 3
                                1 2 3 4 
                                8 7 6 5
                                9 8 7 6))))
     (let* ((h (g 'copy)))
       (h 'set 0 0 9)
       (test-assert "mutated copy does not directly alter original"
        (and
         (equal? (g 'arr) #( 0 1 2 3
                             1 2 3 4 
                             8 7 6 5
                             9 8 7 6))
         (equal? (h 'arr) #( 9 1 2 3
                             1 2 3 4 
                             8 7 6 5
                             9 8 7 6))))))))

;; test3a
((lambda ()
   (letrec ((g   (make-grid 4 4
                            '(0 1 2 3
                                1 2 3 4 
                                8 7 6 5
                                9 8 7 6))))
      (test-assert "width is 4" (= (g 'wid) 4)))))

;; test3b 
((lambda ()
   (letrec ((g   (make-grid 4 4
                            '(0 1 2 3
                                1 2 3 4 
                                8 7 6 5
                                9 8 7 6))))
     (test-assert "height is 4" (= (g 'hgt) 4)))))

;; test3c 
((lambda ()
   (letrec ((g   (make-grid 4 4
                            '(0 1 2 3
                                1 2 3 4 
                                8 7 6 5
                                9 8 7 6))))
     (test-assert "zero at top left" (= (g 'get 0 0) 0)))))

    

;; test 3 
((lambda () 
    (letrec ((g   (make-grid 4 4
                             '(0 1 2 3
                               1 2 3 4 
                               8 7 6 5
                               9 8 7 6))))
      (test-assert "grid get"  (= (g 'wid) 4))
      (test-assert "grid get"  (= (g 'hgt) 4))

      (test-assert "grid get"  (= (g 'get 0 0) 0))
      (test-assert "grid get"  (= (g 'get 1 0) 1))
      (test-assert "grid get"  (= (g 'get 2 0) 2))
      (test-assert "grid get"  (= (g 'get 3 0) 3))

      (test-assert "grid get"  (= (g 'get 0 1) 1))
      (test-assert "grid get"  (= (g 'get 1 1) 2))
      (test-assert "grid get"  (= (g 'get 2 1) 3))
      (test-assert "grid get"  (= (g 'get 3 1) 4))

      (test-assert "grid get"  (= (g 'get 0 2) 8))
      (test-assert "grid get"  (= (g 'get 1 2) 7))
      (test-assert "grid get"  (= (g 'get 2 2) 6))
      (test-assert "grid get"  (= (g 'get 3 2) 5))

      (test-assert "grid get"  (= (g 'get 0 3) 9))
      (test-assert "grid get"  (= (g 'get 1 3) 8))
      (test-assert "grid get"  (= (g 'get 2 3) 7))
      (test-assert "grid get"  (= (g 'get 3 3) 6))
      )))
);; test-group GRID TESTS




