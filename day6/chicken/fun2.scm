


;; how do we start with chicken evens ??
(import (chicken pretty-print))
(import (chicken format))

;; hash tables
(import (srfi-69)) 

;; format 
;; pp defined by pretty-print
(define fmt format)


;; make a 2 d grid from a list of strings is common task 
;; creates a fresh grid each time example is called 
(define example 
  (lambda ()
   (vector
    "....#....."
      ".........#"
      ".........."
      "..#......."
      ".......#.."
      ".........."
      ".#..^....."
      "........#."
      "#........."
      "......#...")))


(define big
  (lambda ()
    (vector    
  "........#...........................#...........#............................................................................#...."
  "...........#..............................................##.#....................##.......#.............##..............#........"
  "...................#..#........#..#..##.................#......##..................#.....#....#.............#...........#...##...."
  "..........#......#..................................................#...................#........#..#.....#......................."
  ".........#...#......#...................................#............#...#........#.............#..#........................#....."
  "..##.#...........#......#.....#.#...#..............#.#......#.......#..............#.............#..................#.......#....."
  "...#.................................####...........#..........#..#.....#........#...........................................#...."
  "..........#......................#............................#....................#..............#......#....#.................#."
  "...#.................#.........#.........#.#.....#....#.................................................................#.#......."
  ".......#.......................#...........#..................................#.............#..................#......#.#........."
  "....................#.#.........#.........#................................#........#.#.........................#..#....#........."
  "........#.............#....#.....#................................##.....#..................#.....................#..............."
  "..............#...................#..........................................#..........#........................#................"
  "...........#...................................................................................#..........#.........##.#.........."
  "............#....................#...................#.................................#.......#............#.......#............."
  "..........................................#.....#.............#.#.......................##......#.....#..........................."
  "........................#...#..................#......................................................#.#..#......................"
  "...#.....#.....................#..........................................#................................................#.....#"
  "..##....................................#.....................#.............#.......#............................................."
  "........#..................#......#................................#.......................................................#......"
  ".........#................#...............................##.#.....................#...........................#...........#.#...."
  "............#.....#..................................#.....#........#................#............................................"
  ".....................................................##..............#.....#.....#.......#........................................"
  "..........#..#..............#..#...#.......................#..........................................#..........................."
  "...............#.............................................................#..#......#.......#..#....................#.........."
  ".#......................#....##.#....................#..#...#............................#............................#....#....#."
  "...#...................................................................................................................#.......#.."
  ".#....#..............#....................#......................................#.......................#....#..#................"
  "........#.....................#............................................#...............#......................................"
  "....#........#..........................................................................#..............................###.......#"
  "........#..#.....................#..................#..................##..........#.............................................."
  "..........................#...#................#....................................................#..#............#............."
  "......#..............#................................#.....................#.................................#............#......"
  "...................................................#....................#.................#.....#................................#"
  "..........................#...............#....#........................#.............................................#..........."
  "##..............#..............................#....#.......................#.................................................#..."
  "...........................#.............................................................#.#................#......#........#.##.."
  "........#..................#...#..............#..............................#.....#..........#..................................."
  "..........#......#.##..#.....#.......##.....#......................................................#..................#..........."
  "....#.........#................#............................................#..#...#............#.....#..#........................"
  "#.......#..............#.....................#..........#....#......#......................................#........#............."
  ".........#..........................................................................#.........#............................#......"
  "..........................#...#.......#...............#..............#.......................#...#................................"
  ".......................................................................#.........................................#...............#"
  "...................#.....#.......................................................#.............................................#.."
  "...##.....................#....#................................#........#...#....#.......##......................................"
  ".................................................................#.......#................................#.........#............."
  "#..........................#.#........................#............................................#...#.........#................"
  "................#.#........#...................#...............................#.........#............#.........#................."
  "...#...................................#.....................................................................#..#......#.........."
  ".................................................#..........................................#....#................................"
  "..............##.#...............................................#......................................................##........"
  "...........#..............................................#.....................................##.....#.........#................"
  "#.......................#..........................#......#...................#.#................................................."
  "..............##..........................#............................................................#................#........."
  ".......................#...#.........#....................................................................#............#.........."
  ".....#.................##.................................................#...........#..............#....#.................#....."
  ".........#................................................#....................#.......#.........................##.#............."
  "...........#............#...#...................#.........#....................................................#.................."
  "....................#.....................................................................#..........#.#.........................."
  "............................##..#.......................#.......#...................#.#..........#..........................#...#."
  "..................#...........#.......#.............................................................#............................."
  "...##......................#....##..................................#...............#.#..#........................................"
  "..#..............................##..................................................................#.......#...................."
  "...#.......#..........#.#.................................................##....#...............#...............#..........#.....#"
  ".....................................................##....................#..#..#...........................#...................."
  "...#......................#........#..................#.##..............#..............#....................................#....."
  "...............#..........................#..........................................................#.#..#..............#........"
  "....................##....................#..#...................................#................................................"
  "...........................#..............#.....................................#.........................##......................"
  "...............#.................................................................#.....................#.........................."
  "......#..........#...#...........#................................................#...#.................................#..#......"
  "..........#............................#.............#.....................#........#............#.........#......................"
  "...........#...........#..........#....#......................................................................#........#.........#"
  "......................#...............##.................................................................................#.....#.."
  "...#................#........#..........#...................#............^...........#.................#.#......##.#.............#"
  ".......................##..............#......#.....................................#.....#.........................#.#.#........."
  ".................#..#..........................................#...................#...#.........#................................"
  "..#.#.....#.............................................................#.#......................................................."
  "..............................#...............###...............#..........##..........................#...#..#..........##......."
  ".................................#.....#.........#......................................................#................#........"
  "...............##.#..................................................#........................#...................#.........#..##."
  ".........#.................................#......#..............................................................................."
  "..................#......................#.#.........................................................#.....#......................"
  ".#..................#.............................#..............................................................................."
  "...................#...................#.#........#......#...........................#......................#..........#.........."
  ".............#...................................................................................................#................"
  "..........................#................................................#........#..#.........................................."
  ".#........................................................#.......#......#..#....................................................."
  "..................##..............#.............#................#...............................#......#......#........#....#.#.."
  "#.............#....................................#.....................................................#........................"
  "............................#................................................................#....#.....#................#........"
  ".........#.......................................................................................................................#"
  "..#.........................................................................................#........#.#...........#.............."
  "...........#......................#.......#................................#...#.......#...............#.........................."
  ".......................#...#..#.......#..................#........#.....#.........#..............................................."
  "..#..##...................#..........#................................................#.........#...................#............."
  "........................##..........................#.......#..........................#...#.#...................................."
  "..........................................................................................#.............#...........#............#"
  "..............................#.............................................................................................#.#..#"
  ".............#..........#...........................#.........................#........#.....................#............#......#"
  "....................................#..........................................................................................#.."
  ".........#.................................#...........................................................................#.........."
  ".#..............................#......#......#.#................................#.................#......#....#..#..#...........#"
  ".#...#................#.......#............#.........#...#..........#......#.........................#....................#......."
  "..............#........................#................................................................................#........."
  ".........#........#..#.........#........#......#..#..............................................#................#..............."
  "...........#......................#.......#..................................#....#...#......#..........#........................."
  "...........#........................................................#.#...........#................#.............................."
  ".#...........................#...................#...#..................................................................#........#"
  "....................................................................#...................#.....#......#...........................#"
  "...#..#..............#......................#......#..#...............#..............#..............#..............#.............."
  "...........#........................#............#.#...........#.................................................................."
  "............#........................#.................................................#..............#.#....................#...."
  "............................#...........................#............................#........#..........................#........"
  "#................................................................#...........................#.............#.#........#.#.......#."
  "............#..........#..............#.......#...#................................#.........#.#................#........#....#..."
  "...#.....................#..........#.............#....................................................#.#........................"
  "...............#..................................................#......................#...............#........................"
  "......#............................#..................#.....#...........................................................#...#.#..."
  "#................#..........................................................................................#..........#.......#.."
  ".................................................................#..............#...................#...................#.#......."
  "...............#..............................................#.................#...............#...........................#....."
  ".............................#...#........................#....................#........#.....................................#.##"
  "...............##..#.....#..#....#.#...#...........................................#.#........#........#.......#.................."
  ".......#........................#....#..#............................................................#.......#...................."
  "........#....................#........#....#.................#........................#.............#............................."
  "#...#...........#......................................................#...............#...........#..#..#....#................#.."
  "................................#........................................................#...#..#......#....#.....#....#...#......"
  "..#..............#............................#........#....#......#.................................................#............")))


;; (define example-height (vector-length (example)))
;; (define example-width (string-length (vector-ref (example) 0)))
;; 
;; (define big-height (vector-length (big)))
;; (define big-width (string-length (vector-ref (big) 0)))
;; 
;; ;; make a grid 130 x 130 contains 0
;; (define (get-xy g x y) (string-ref (vector-ref g y) x))

;; loop from .. to .. increment by 1...call fn each time 
(define (iter from to fn)
  (letrec ((help (lambda (i)
                   (cond
                    ((> i to) #f)
                    (#t (fn i)
                        (help (+ i 1)))))))
    (help from)))

;; ;; functional loop  using power of functions - no macro difficulties
;; (define big-x 0)
;; (define big-y 0)
;; (define (find-big-start)
;;   (let ((g (big)))
;;     (iter 0 129 (lambda (y)
;;                   (let ((vec (vector-ref g y)))
;;                     (iter 0 129 (lambda (x)
;;                                   (let ((ch (string-ref vec x)))
;;                                   ;; (display "x = ")(display x)
;;                                   ;; (display " y = ")(display y)
;;                                     (if (char=? ch #\^)
;;                                         (begin (set! big-x x)(set! big-y y))
;;                                         #f)                                     
;;                                     (display ch))))
;;                     (newline))))))
;; 
;; ;; show big grid and set big-x big-y
;; (find-big-start)



;; big safe from destructive as creates a new vector (vector of the string)
;; chars 
;; . empty 
;;  # obstacle 
;; ^ start location
;; O temporary obstacle
(define (make-obj fn) 
  (let* ((src #f) ;; (big) external vector of strings read to create a big object
         (wid #f)
         (hgt #f)
         (start-x 0)
         (start-y 0)
         (dir 'north)
         (pos #f)
         (vec #f))
    (letrec ((init (lambda ()
                     (set! src (fn)) ;; (big) external vector of strings read to create a big object
                     (set! wid (vector-length src))
                     (set! hgt (string-length (vector-ref src 0)))
                     (set! start-x 0)
                     (set! start-y 0)
                     (set! dir 'north)
                     (set! pos (vector start-x start-y))
                     (set! vec (make-vector hgt))
                     (iter 0 (- hgt 1) (lambda (y)
                                         (let ((vec2 (make-vector wid))
                                               (svec (vector-ref src y)))
                                           (vector-set! vec y vec2)
                                           (iter 0 (- wid 1) (lambda (x)
                                                               (let ((ch (string-ref svec x)))
                                                                 (vector-set! vec2 x ch)
                                                                 ;; ;; (display "x = ")(display x)
                                                                 ;; ;; (display " y = ")(display y)
                                                                 (if (char=? ch #\^)
                                                                     (begin (set! start-x x)
                                                                            (set! start-y y))
                                                                     #f)                                              
                                                                 ;;(display ch)
                                                                 )))
                                           ;;(newline)
                                           )))))
             ;; count number of squares visited 
             (get-square-count 
              (lambda ()
                (let ((count 0))
                (iter 0 (- hgt 1) 
                      (lambda (y)
                        (let ((vec2 (vector-ref vec y)))
                          (iter 0 (- wid 1) 
                                (lambda (x)
                                  (let ((ch (vector-ref vec2 x)))
                                    (cond
                                     ((or (eq? ch #\X)(eq? ch #\^))
                                      (set! count (+ count 1))))))))))
                count)))

             ;; clear clears path and obstacle
             (clear 
              (lambda ()
                (iter 0 (- hgt 1) 
                      (lambda (y)
                        (let ((vec2 (vector-ref vec y)))
                          (iter 0 (- wid 1) 
                                (lambda (x)
                                  (let ((ch (vector-ref vec2 x)))
                                    (cond
                                     ((or (eq? ch #\X)(eq? ch #\O))
                                      (vector-set! vec2 x #\.)))))))))))
              
             ;; raw x y setting - may corrupt vector !!!
             (get (lambda (x y) ;; no bounds checks !!!
                    (vector-ref (vector-ref vec y) x)))
             (set (lambda (x y z) ;; no bounds checks !!!
                    (vector-set! (vector-ref vec y) x z)))

             (rock (lambda (x y) ;; no bounds checks !!!
                    (set x y #\O)))

             ;; stamp foot where we are 
             (stamp (lambda (p)  
                      (let ((x (vector-ref p 0))
                            (y (vector-ref p 1)))
                        (let ((ch (get x y)))
                          (cond
                           ((eq? ch #\^) #f)
                           ((eq? ch #\O) (error (fmt "tried stamp on obstacle O at pos ~a~%" p)))
                           ((eq? ch #\#) (error (fmt "tried stamp on obstacle # at pos ~a~%" p)))
                           ((eq? ch #\X) #f) ;; already stamped
                           ((eq? ch #\.) (set x y #\X))
                           (#t (error (fmt "stamp bad char ~a~%" p))))))))
             ;;
             (off-map? (lambda (p) ;; off map?
                         (let ((x (vector-ref p 0))
                               (y (vector-ref p 1)))
                           (or (< x 0)(>= x wid)(< y 0)(>= y hgt)))))
             (on-map? (lambda (p) (not (off-map? p))))
             (north-of (lambda (p) 
                         (let ((x (vector-ref p 0))
                               (y (vector-ref p 1)))
                           (vector x (- y 1)))))
             (south-of (lambda (p) 
                         (let ((x (vector-ref p 0))
                               (y (vector-ref p 1)))
                           (vector x (+ y 1)))))
             (west-of (lambda (p) 
                         (let ((x (vector-ref p 0))
                               (y (vector-ref p 1)))
                           (vector (- x 1) y))))
             (east-of (lambda (p) 
                         (let ((x (vector-ref p 0))
                               (y (vector-ref p 1)))
                           (vector (+ x 1) y))))
             (obstacle? (lambda (p) ;; # and O are obstacles : X visited ^ start 
                         (let ((x (vector-ref p 0))
                               (y (vector-ref p 1)))
                           (let ((ch (get x y)))
                             (cond
                              ((char=? ch #\.) #f)
                              ((char=? ch #\#) #t)
                              ((char=? ch #\X) #f)
                              ((char=? ch #\^) #f)
                              ((char=? ch #\O) #t)
                              (#t (error (fmt #t "obstacle: bad char ~a " ch))))))))
             (run (lambda () 
                    (init)
                     (set! pos (vector start-x start-y))
                    (next))) ;; call next
             (next (lambda () 
                     ;;(fmt #t "guard at ~a ~%" pos)
                     (cond
                      ((off-map? pos) (fmt #t "off map at ~a ~%" pos))
                      (#t (cond
                           ((eq? dir 'north) (next-north))
                           ((eq? dir 'east) (next-east))
                           ((eq? dir 'west) (next-west))
                           ((eq? dir 'south) (next-south))
                           (#t (fmt #t "bad dir ~a " dir)))))))
             (next-north (lambda ()
                           (let ((p2 (north-of pos)))
                             (cond
                              ((off-map? p2) (fmt #t "off map at ~a ~%" p2))
                              ((obstacle? p2) ;; turn east
                               (set! dir 'east)
                               (next))
                              (#t ; go 
                               (stamp p2)
                               (set! pos p2)
                               (next))))))
             (next-south (lambda ()
                           (let ((p2 (south-of pos)))
                             (cond
                              ((off-map? p2) (fmt #t "off map at ~a ~%" p2))
                              ((obstacle? p2) ;; turn
                               (set! dir 'west)
                               (next))
                              (#t ; go 
                               (stamp p2)
                               (set! pos p2)
                               (next))))))
             (next-east (lambda ()
                           (let ((p2 (east-of pos)))
                             (cond
                              ((off-map? p2) (fmt #t "off map at ~a ~%" p2))
                              ((obstacle? p2) ;; turn
                               (set! dir 'south)
                               (next))
                              (#t ; go 
                               (stamp p2)
                               (set! pos p2)
                               (next))))))
             (next-west (lambda ()
                           (let ((p2 (west-of pos)))
                             (cond
                              ((off-map? p2) (fmt #t "off map at ~a ~%" p2))
                              ((obstacle? p2) ;; turn
                               (set! dir 'north)
                               (next))
                              (#t ; go 
                               (stamp p2)
                               (set! pos p2)
                               (next))))))
             ;; show internal one 
             (show (lambda ()
                     (newline)
                     (iter 0 (- hgt 1) 
                           (lambda (y)
                             (let ((vec2 (vector-ref vec y)))
                               (iter 0 (- wid 1) 
                                        (lambda (x)
                                          (let ((ch (vector-ref vec2 x)))
                                            (fmt #t "~a" ch)
                                            )))
                               (newline))))))

             ;; internal definitions
             )
      ;; 
      (init)
      ;; return fn takes arguments and does some computation
      (lambda (op . args)
        (cond
         ((eq? op 'run) (run))
         ((eq? op 'show) (show))
         ((eq? op 'init) (init))         
         ((eq? op 'start) (list start-x start-y))
         ((eq? op 'size) (list wid hgt))
         ((eq? op 'count) (get-square-count))
         ((eq? op 'clear) (clear))
         
         ((eq? op 'rock) (let ((dx (car args))
                                   (dy (car (cdr args))))
                               (rock dx dy)
                          ;;(fmt #t "asked for ~a ~a : ~a ~%" dx dy (get dx dy))
                               ))
         
         
         ;;((eq? op 'show) (list big-x big-y))
         ((eq? op 'get) (let ((dx (car args))
                              (dy (car (cdr args))))
                          (get dx dy)
                          ;;(fmt #t "asked for ~a ~a : ~a ~%" dx dy (get dx dy))
                          ))
         (#t (fmt #f "unknown message ~a with args ~a ~%" op args)))))))


(define (make-big) 
  (make-obj big))

(define (make-small) 
  (make-obj example))


(define (part-1)
  (let ((p (make-big)))
    (p 'run)
    (p 'count)))




  




#|
  

;; use a macro 
(define-syntax swap!
  (er-macro-transformer
    (lambda (form rename compare?)
      (let (
        (x (cadr form))
        (y (caddr form))
        (%tmp (rename 'tmp))
        (%let (rename 'let))
        (%set! (rename 'set!))
        )
        `(,%let ((,%tmp ,x))
           (,%set! ,x ,y)
           (,%set! ,y ,%tmp))))))


;;(pp (expand '(swap! x y)))

;; dolist ?
;; iterate over a list -> iterate over the string
(define grid-from-strings
  (lambda (strings)    
    (let* ((width (string-length (car strings)))
           (height (length strings))
           (start-x 0)
           (start-y 0)
           (grid (make-vector (+ 1 height))))
      (letrec ((iter (lambda (xs y) ;; the list 
                       (cond
                        ((null? xs) #f)
                        (#t (let ((x 1))
                              (let ((vec (vector-set! grid y (make-vector (+ 1 width)))))
                              (iter2 (car xs) 0 (string-length (car xs)) x y vec)
                              (iter (cdr xs) (+ y 1))))))))
               ;; i index into srting - zero based 0 = 1st char of string
               ;; vec is a 1 based index vector
               (iter2 (lambda (str i lim x y vec) 
                        (cond
                         ((= i lim) #f)
                         (#t   
                          (let ((ch (string-ref str i)))                            
                            (cond
                             ((char=? ch #\#)
                              (vector-set! vec x 'rock))
                             ((char=? ch #\O)
                              (vector-set! vec x 'obstacle))
                             ((char=? ch #\.)
                              (vector-set! vec x 'empty))
                             ((char=? ch #\^)
                              (set! start-x x)
                              (set! start-y y)
                              (vector-set! vec x 'empty))
                             (#t (display "unrecognised character in grid at")
                                (display x)
                                (display "")
                                (display y))))
                            
                            (if (> x width) (set! width (+ width 1)) #f)
                            (if (> y height) (set! height (+ height 1)) #f)
                            
                            ;; loop
                          (iter2 str (+ i 1) lim (+ x 1) y vec))))))
        (let ((y 1))
          (iter strings y)   
          (vector-set! grid 0 (list->vector start-x start-y width height))
          grid)))))


;; raw x y
(define (grid-x-y g x y)
  (vector-ref (vector-ref g y) x))

(define (grid-width g)  (grid-x-y g 2 0))
(define (grid-height g)  (grid-x-y g 3 0))
(define (grid-start-x g)  (grid-x-y g 0 0))
(define (grid-start-y g)  (grid-x-y g 1 0))

(define display-grid
  (lambda (g p dir visited)    
    (let ((px (vector-ref p 0))
          (py (vector-ref p 1))
          (width (grid-width g))
          (height (grid-height g))
          (start-x (grid-start-x g))
          (start-y (grid-start-y g)))
      (newline)
      (letrec ((iter (lambda (x y) ;; the list 
                       (cond
                        ((> y height) (newline) #f)
                        ((> x width) (newline) (iter 1 (+ y 1)))     
                        (#t 
                         (let ((elem (grid-x-y g x y)))
                           (cond
                            ((eq? elem 'rock) (display "X"))
                            ((eq? elem 'obstacle) (display "O"))
                            ;; ((member (list x y) visited)
                            ;;  (display "Y"))
                            ((eq? elem 'empty) (display "."))
                            (#t 
                             (display "unrecognised character in grid at")
                             (display x)
                             (display "")
                             (display y)))

                           (cond
                            ((and (= start-x x)(= start-y y))
                             (display "*"))
                            (#t (display" ")))

                           (cond
                            ((and (= px x)(= py y))
                             (cond
                              ((eq? dir 'north) (display "^"))
                              ((eq? dir 'east) (display ">"))
                              ((eq? dir 'south) (display "v"))
                              ((eq? dir 'west) (display "<"))
                              (#t (display "unrecognised player direction grid at")
                                  (display x)
                                  (display "")
                                  (display y)
                                  (display " with direction dir : ")
                                  (display dir)
                                  )))
                            (#t (display" ")))
                           
                           (iter (+ x 1) y)))))))
        (let ((x 1)(y 1))
          (iter 1 1))))))



(define (test)
  (let ((g (example)))
    (display-grid g #(0 0) '() )))

|#










         







