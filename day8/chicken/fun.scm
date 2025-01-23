

(import (chicken format))
(define fmt format)

(import (simple-loops))

(import (srfi-69))

(define problem-data #(
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


(define example-data #("............"
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

(define example-width (string-length (vector-ref example-data 0)))
(define example-height (vector-length example-data))

(define problem-width (string-length (vector-ref problem-data 0)))
(define problem-height (vector-length problem-data))

(define width problem-width)
(define height problem-height)
(define data problem-data)

(define use-example 
  (lambda ()
    (set! width example-width)
    (set! height example-height)
    (set! data example-data)))


(define use-problem
  (lambda ()
    (set! width problem-width)
    (set! height problem-height)
    (set! data problem-data)))
    
    
;; choose problem
;;(use-example)
(use-problem)


      

;; (define width example-width)
;; (define height example-height)
;; (define data example)




;; can we time how long it takes to run this routine ?
(define get-xy 
  (lambda (x y)
    (string-ref (vector-ref data y) x)))


#|

iterate over grid 
  find a letter A 
      iterate over rest of grid until find another A
          once have two coordinates then can compute where 

iterate over grid once and collect where locations of all symbols are that are not empty spaces 

collection symbols 
for each symbol have a collection of locations 

from this need to compute the location of the antinodes 


could have a hash table with characters as key 
    #\a ->> ((0 0) (2 2)(3 3) ...)


|#

(define myhash (make-hash-table))

(define collect-data 
  (lambda ()
    (set! myhash (make-hash-table))
      (do-for (y 0 height 1)
              (do-for (x 0 width 1)
                      (let ((ch (get-xy x y)))
                        (cond
                         ((char=? ch #\.) #f)
                         (#t 
                          (let ((cap (hash-table-ref/default myhash ch #f))
                                (coord (list x y)))
                            (cond
                             ((eq? cap #f)  
                              (hash-table-set! myhash ch (list coord)))
                             (#t
                              (hash-table-set! myhash ch (cons coord cap))))))))))))

;; imperative
(collect-data)

(define show-keys
 (lambda ()
   (hash-table-for-each myhash (lambda (key val)
                                 (fmt #t "~a : ~a~%" key val)))))

;; anti nodes 
(define mynodes (make-hash-table))


#|

for each list of coordinates in key , compute a list of anti nodes 
 enter this into mynodes - recording the key in the list

|#

(define da 
  (lambda (x1 y1 x2 y2)
    (let ((dx (- x2 x1))
          (dy (- y2 y1)))
      (list (- x1 dx)
            (- y1 dy)))))

(define db
  (lambda (x1 y1 x2 y2)
    (let ((dx (- x2 x1))
          (dy (- y2 y1)))
      (list (+ x2 dx)
            (+ y2 dy)))))   

(da 3 3 7 6)
(db 3 3 7 6)

(db 7 6 3 3)
(da 7 6 3 3)

(define on-map? 
  (lambda (x y)
    (and (>= x 0) (>= y 0)
         (< x width) (< y height))))

(define found-antinode 
  (lambda (char coord)
    (let ((cap (hash-table-ref/default mynodes coord #f)))
      (cond
       (cap (cond 
             ((not (member char cap))
              (hash-table-set! mynodes coord (cons char cap)))))
       (#t (hash-table-set! mynodes coord (list char)))))))



(define compute-nodes 
  (lambda ()
    (set! mynodes (make-hash-table))
    (letrec ((iter (lambda (xs fn)
                     (cond
                      ((null? xs) #f)
                      (#t (iter2 (car xs) (cdr xs) fn)
                          (iter (cdr xs) fn)))))
             (iter2 (lambda (a xs fn)
                      (cond
                       ((null? xs) #f)
                       (#t (let ((b (car xs)))
                             (fn a b)
                             (iter2 a (cdr xs) fn)))))))

      (hash-table-for-each myhash 
                           (lambda (key val)
                             (let ((coords val)) ;; for each pair of coords 
                               (iter coords (lambda (c1 c2)
                                              (let ((x1 (car c1))
                                                    (y1 (car (cdr c1)))
                                                    (x2 (car c2))
                                                    (y2 (car (cdr c2))))
                                                (fmt #t "<~a> : ~a ~a : ~a ~a : ~a ~a~%" key c1 c2
                                                     x1 y1
                                                     x2 y2)

                                                ;; antinode 1  DA 
                                                (let* ((z (da x1 y1 x2 y2))
                                                       (x (car z))
                                                       (y (car (cdr z))))
                                                  (cond
                                                   ((on-map? x y) (found-antinode key z))))

                                                ;; antinode 2  DB 
                                                (let* ((z (db x1 y1 x2 y2))
                                                       (x (car z))
                                                       (y (car (cdr z))))
                                                  (cond
                                                   ((on-map? x y) (found-antinode key z))))
                                                )))))))))

;; compute the antinodes
(compute-nodes)


(define show-antinodes
  (lambda ()
    (let ((singles 0)
          (non-singles 0))
      (hash-table-for-each mynodes (lambda (key val)
                                     (fmt #t "~a : ~a : ~a ~%" key val (length val))
                                     (cond
                                      ((> (length val) 1) 
                                       (set! non-singles (+ 1 non-singles)))
                                      (#t (set! singles (+ 1 singles))))))
      (fmt #t "there are ~a single antinodes and ~a non single antinodes ~% for a total of ~a antinodes ~%" singles non-singles (+ singles non-singles)))))


(show-antinodes)



























