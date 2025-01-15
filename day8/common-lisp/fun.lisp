
(ql:quickload :uiop)

(uiop:define-package #:aoc
  (:use #:cl
        #:uiop))

(in-package #:aoc)

(defparameter file (let ((xs (uiop:read-file-lines "../input.txt")))
                     (make-array (length xs) :initial-contents xs)))

(defparameter wid (length (aref file 0)))
(defparameter hgt (length file))

;; same stuff for 2d grid
(defun on-map (x y)
  (and (>= x 0)(< x wid)
       (>= y 0)(< y hgt)))

(defun off-map (x y) (not (on-map x y)))

(defparameter anti-nodes '())

;; randomly look for any letter not a dot . 
;; iterate over the list of letters found , look for another matching letter in another square
;; when find a match - figure out where anti nodes go

;; look for all letters and collect them 
(defun find-letters ()
  (let ((letters '()))
    (loop for y from 0 to (- hgt 1) do
      (let ((row (aref file y)))
        (loop for x from 0 to (- wid 1) do 
          (let ((ch (aref row x)))
            (cond
              ((char= ch #\.) nil)
              ((member ch letters) nil)
              (t (setq letters (cons ch letters))))))))
    letters))

;; get all locations of a specific letter
(defun find-locations (ch2)
  (let ((locations '()))
  (loop for y from 0 to (- hgt 1) do
    (let ((row (aref file y)))
      (loop for x from 0 to (- wid 1) do 
        (let ((ch (aref row x)))
          (cond
            ((char= ch ch2) (setq locations (cons (list x y) locations))))))))
    locations))

#|

0 1 2 3 4 5
1 x
2     x
3
4
5

can we visually check this 


|#

;; given all locations of letter h 
;; iterate through list making pairs 
(defun pair-locations (xs)
  (let ((i 0)(j 0)(lim (length xs))(pairs '()))
    (loop for i from 0 to (- lim 1) do
      (let ((a (nth i xs)))
        (loop for j from 1 to (- lim 1) do
          (let ((b (nth j xs)))
            (let ((ax (car a))(ay (car (cdr a)))
                  (bx (car b))(by (car (cdr b))))
              (let ((dx (- ax bx))
                    (dy (- ay by)))
                
                  
              
            


