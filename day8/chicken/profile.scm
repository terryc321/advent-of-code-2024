

(import (chicken format))
(define fmt format)

(import (simple-loops))

;; on small lists vs vectors no difference

(define test-bench 
  (lambda ()

(define problem #(
"..................................D..............."
"..o.............................D..............b.."
".H....o........z...........D......................"
".......................Y...O............1........."
"......4...............N....................d......"
"........4......................D..........d......."
".o.............4................1................."
".......................................y.1......w."
"..........Y.H..........5.........y.......b........"
"...Q.....H......Y...............y..m.............."
".........Q.Y..............O..................m...."
"............................N...O................."
"....5.............................8...........b..."
"....H....................................w........"
".........................1.....O..y.........d....."
".........................4........................"
"...............................2.................."
"........Q...5....................................."
"..............0..................................."
".....................0...Z...2...................."
".....0.........................MZ.m..............."
"...6........0...........Z.....m..................d"
".................................B.E.............."
"......6..........................................."
".......................................8..B......."
"..z......5.............................7.....8...."
".................................................."
"......................................2..........."
".........Z..6....................q................"
".................................................."
".......6.................G..7....................."
".......z................I.....b.B...............e."
".........N..............................2...M....."
"..G..............................................."
"...................w...g.....7......E............."
"...........................q...n..........M.E....."
"........................................I........."
"................................W.......7..e......"
"..........................9....................W.."
"...............G.3..8..............B.9.......i...."
"..................................n..............3"
"....................3................E............"
"............................3........h..i........."
"....................9......hi...............n..M.."
"..............z..............I....h..............i"
"..............h............g......................"
"....................G..............e.............."
"...............................I............g....."
"................q...g..9..........e...W......n...."
".......................................W.........."
))


(define example #("............"
                  "........0..."
                  ".....0......"
                  ".......0...."
                  "....0......."
                  "......A....."
                  "............"
                  "............"
                  "........A..."
                  ".........A.."
                  "............"
                  "............"))

(define example-width (string-length (vector-ref example 0)))
(define example-height (vector-length example))

(define problem-width (string-length (vector-ref problem 0)))
(define problem-height (vector-length problem))

(define width problem-width)
(define height problem-height)
(define data problem)

;; (define width example-width)
;; (define height example-height)
;; (define data example)


;; can we time how long it takes to run this routine ?
(define get-xy 
  (lambda (x y)
    (string-ref (vector-ref data y) x)))


;; loop from 0 to 12 Y
;; loop from 0 to 12 X 
;; 10,000 times takes half a second
;; 100,000 times should be 5 seconds 
(define example-profile 
  (lambda ()
    (let ((lim 100000))
      (do-for (z 0 lim 1)
              ;;(fmt #t "z = ~a ~%" z )
              (let ((char-dots 0)
                    (char-as 0)
                    (char-zeros 0))
                (do-for (y 0 height 1)
                        (do-for (x 0 width 1)
                                (let ((ch (get-xy x y)))
                                  (cond
                                   ((char=? ch #\.) (set! char-dots (+ 1 char-dots)))
                                   ((char=? ch #\0) (set! char-zeros (+ 1 char-zeros)))
                                   ((char=? ch #\A) (set! char-as (+ 1 char-as)))
                                   (#t (fmt "unknown char ~a~%" ch))))))
                (cond
                 ((>= z (- lim 1))
                  (fmt #t "dots ~a : as  ~a : zeros  ~a : ~%" char-dots char-as char-zeros)))
                )))))



;; 
(example-profile)
)) ;; test bench

;; (time (test-bench))
;;======================================================================================
;; 2nd test bench 
;; this time using lists 

(define test-bench2
  (lambda ()

(define row-ref list-ref) ;; list access
(define row-length length) ;; list length 

;; instead of vector-ref example N   list-ref 
;; 
(define example '("............"
                  "........0..."
                  ".....0......"
                  ".......0...."
                  "....0......."
                  "......A....."
                  "............"
                  "............"
                  "........A..."
                  ".........A.."
                  "............"
                  "............"))

(define example-width (string-length (row-ref example 0)))
(define example-height (row-length example))

;; can we time how long it takes to run this routine ?
(define get-xy 
  (lambda (x y)
    (string-ref (row-ref example y) x)))


;; loop from 0 to 12 Y
;; loop from 0 to 12 X 
;; 10,000 times takes half a second
;; 100,000 times should be 5 seconds 
(define example-profile 
  (lambda ()
    (do-for (z 0 100000 1)
    (let ((char-dots 0)
          (char-as 0)
          (char-zeros 0))
    (do-for (y 0 12 1)
            (do-for (x 0 12 1)
                    (let ((ch (get-xy x y)))
                      (cond
                       ((char=? ch #\.) (set! char-dots (+ 1 char-dots)))
                       ((char=? ch #\0) (set! char-zeros (+ 1 char-zeros)))
                       ((char=? ch #\A) (set! char-as (+ 1 char-as)))
                       (#t (fmt "unknown char ~a~%" ch))))))
    (cond
     ((> z 9998)
      (fmt "dots ~a : as  ~a : zeros  ~a : ~%" char-dots char-as char-zeros)))))))

;; 
(example-profile)
)) ;; test bench





















