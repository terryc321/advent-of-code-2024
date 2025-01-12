

(defpackage #:aoc
  (:use #:cl))

(in-package #:aoc)

;; enable full debugging capabilties
(declaim (optimize (speed 0) (space 0) (debug 3)))


;; 140 x 140 word search 
(defparameter *grid* 
  (let ((filename (merge-pathnames "input.txt" 
                                   (truename (merge-pathnames "../" (uiop:getcwd)))))
        (seq '()))
    (with-open-file (in filename)
      (handler-case 
          (loop while t do
            ;;(/ 1 0)
            (let ((s (read in)))
              (format t "got ~a~%" s)
              (setq seq (cons (format nil "~a" s) seq))))
        (end-of-file (c) 
          t
          (setq seq (reverse seq))
          (let ((result (make-array (length seq))))
            (loop for i from 0 to (- (length seq) 1) do
              (setf (aref result i) (nth i seq)))
            result)
          ;;(format t "condition caught = ~a~%" c)
          )
        (error (c) (format t "some other error caught = ~a~%" c))))))



(defun char-at (ch x y)
  ;; check in bounds 
  (handler-case
      (char= ch (aref (aref *grid* y) x))
    (error (c) nil)))


(defun cat (x y)
  ;; check in bounds 
  (handler-case
      (aref (aref *grid* y) x)
    (error (c) nil)))






#|

X + positive east direction
Y + positive south direction

 east

|#

(defun xmas-north (x y)
  (and (char-at #\X (+ x 0) (+ y 0)) 
       (char-at #\M (+ x 0) (+ y -1))
       (char-at #\A (+ x 0) (+ y -2))
       (char-at #\S (+ x 0) (+ y -3))))

(defun xmas-south (x y)
  (and (char-at #\X (+ x 0) (+ y 0)) 
       (char-at #\M (+ x 0) (+ y 1))
       (char-at #\A (+ x 0) (+ y 2))
       (char-at #\S (+ x 0) (+ y 3))))

(defun xmas-east (x y)
  (and (char-at #\X (+ x 0) (+ y 0)) 
       (char-at #\M (+ x 1) (+ y 0))
       (char-at #\A (+ x 2) (+ y 0))
       (char-at #\S (+ x 3) (+ y 0))))

(defun xmas-west (x y)
  (and (char-at #\X (+ x 0) (+ y 0)) 
       (char-at #\M (+ x -1) (+ y 0))
       (char-at #\A (+ x -2) (+ y 0))
       (char-at #\S (+ x -3) (+ y 0))))


(defun xmas-north-east (x y)
  (and (char-at #\X (+ x 0) (+ y 0)) 
       (char-at #\M (+ x 1) (+ y -1))
       (char-at #\A (+ x 2) (+ y -2))
       (char-at #\S (+ x 3) (+ y -3))))

(defun xmas-south-east (x y)
  (and (char-at #\X (+ x 0) (+ y 0)) 
       (char-at #\M (+ x 1) (+ y 1))
       (char-at #\A (+ x 2) (+ y 2))
       (char-at #\S (+ x 3) (+ y 3))))


(defun xmas-north-west (x y)
  (and (char-at #\X (+ x 0) (+ y 0)) 
       (char-at #\M (+ x -1) (+ y -1))
       (char-at #\A (+ x -2) (+ y -2))
       (char-at #\S (+ x -3) (+ y -3))))

(defun xmas-south-west (x y)
  (and (char-at #\X (+ x 0) (+ y 0)) 
       (char-at #\M (+ x -1) (+ y 1))
       (char-at #\A (+ x -2) (+ y 2))
       (char-at #\S (+ x -3) (+ y 3))))



(defmacro xmas (dir)
  `(progn (format t "XMAs found at ~a,~a in direction ~a~%" x y ,dir)
          (incf tot)))

(defun examine ()
  (let ((tot 0))
    (loop for y from 0 to (- (length *grid*) 1) do
      (loop for x from 0 to (- (length (aref *grid* 0)) 1) do
        ;;(format t "looking at square (~a,~a) : ~a ~%" x y (cat x y))
        (when (xmas-east x y) (xmas 'east))
        (when (xmas-west x y) (xmas 'west))
        (when (xmas-south x y) (xmas 'south))
        (when (xmas-north x y) (xmas 'north))
        (when (xmas-north-east x y) (xmas 'north-east))
        (when (xmas-north-west x y) (xmas 'north-west))
        (when (xmas-south-east x y) (xmas 'south-east))
        (when (xmas-south-west x y) (xmas 'south-west))))
    tot))



(defun test ()
  (let ((*grid* #("..X..."
                  ".SAMX."
                  ".A..A."
                  "XMAS.S"
                  ".X....")))
    ;; (format t "looking at square (~a,~a) : ~a ~%" 0 0 (cat 0 0))
    ;; (format t "looking at square (~a,~a) : ~a ~%" 1 0 (cat 1 0))
    ;; (format t "looking at square (~a,~a) : ~a ~%" 0 1 (cat 0 1))
    (examine)))

(defun test2 ()
  (let ((*grid* #("MMMSXXMASM"
                  "MSAMXMSMSA"
                  "AMXSXMAAMM"
                  "MSAMASMSMX"
                  "XMASAMXAMM"
                  "XXAMMXXAMA"
                  "SMSMSASXSS"
                  "SAXAMASAAA"
                  "MAMMMXMMMM"
                  "MXMXAXMASX")))
    (examine)))

(defun test3()
  (examine))


(defun part-1()
  (examine))
;; => 2153
;; => 2547 forgot east west north south ... 


;; =========================================================================
;; part 2 

#|

x mas

m  
 a 
  s


m . m  new
. a . 
s . s


m . s
. a . 
m . s

    m
  a
s

m . m   duplicate
. a . 
s . s

s . m   new 
. a . 
s . m


    s
     a
      m

s . m   
. a . 
s . m

s . s   new   
. a . 
m . m


       s
     a 
   m 

s . s   
. a . 
m . m

m . s   new
. a . 
m . s


collated 
========================================================================


m . m  new
. a . 
s . s

s . m   new 
. a . 
s . m

s . s   new   
. a . 
m . m

m . s   new
. a . 
m . s

top left is 0 0  , using +x +y 
reading letters m s a from left to right , top to bottom
  (and (char-at #\M (+ x 0) (+ y 0)) 
       (char-at #\S (+ x 2) (+ y 0))
       (char-at #\A (+ x 1) (+ y 1))
       (char-at #\M (+ x 0) (+ y 2))
       (char-at #\S (+ x 2) (+ y 2))))
     

|#


(defun xmas-top (x y)
  (and (char-at #\M (+ x 0) (+ y 0)) 
       (char-at #\M (+ x 2) (+ y 0))
       (char-at #\A (+ x 1) (+ y 1))
       (char-at #\S (+ x 0) (+ y 2))
       (char-at #\S (+ x 2) (+ y 2))))

(defun xmas-right (x y)
  (and (char-at #\S (+ x 0) (+ y 0)) 
       (char-at #\M (+ x 2) (+ y 0))
       (char-at #\A (+ x 1) (+ y 1))
       (char-at #\S (+ x 0) (+ y 2))
       (char-at #\M (+ x 2) (+ y 2))))

(defun xmas-bot (x y)
  (and (char-at #\S (+ x 0) (+ y 0)) 
       (char-at #\S (+ x 2) (+ y 0))
       (char-at #\A (+ x 1) (+ y 1))
       (char-at #\M (+ x 0) (+ y 2))
       (char-at #\M (+ x 2) (+ y 2))))

(defun xmas-left (x y)
  (and (char-at #\M (+ x 0) (+ y 0)) 
       (char-at #\S (+ x 2) (+ y 0))
       (char-at #\A (+ x 1) (+ y 1))
       (char-at #\M (+ x 0) (+ y 2))
       (char-at #\S (+ x 2) (+ y 2))))



(defun examine2 ()
  (let ((tot 0))
    (loop for y from 0 to (- (length *grid*) 1) do
      (loop for x from 0 to (- (length (aref *grid* 0)) 1) do
        
        (when (xmas-top x y) (xmas 'top))
        (when (xmas-right x y) (xmas 'right))
        (when (xmas-bot x y) (xmas 'bot))
        (when (xmas-left x y) (xmas 'left))
        ))
    tot))


(defun test4 ()
  (let ((*grid* #("MMMSXXMASM"
                  "MSAMXMSMSA"
                  "AMXSXMAAMM"
                  "MSAMASMSMX"
                  "XMASAMXAMM"
                  "XXAMMXXAMA"
                  "SMSMSASXSS"
                  "SAXAMASAAA"
                  "MAMMMXMMMM"
                  "MXMXAXMASX")))
    (examine2)))

(defun part-2()
  (examine2))

;; (part-2) => 1939











  













