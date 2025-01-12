

(defpackage #:aoc
  (:use #:cl))

(in-package #:aoc)

(declaim (optimize (speed 0) (space 0) (debug 3)))


;; 1000 entries 
(defun read-values ()
  (let ((array (make-array 1002 :initial-element nil))
        (filename (merge-pathnames "input.txt" 
                                   (truename (merge-pathnames "../" (uiop:getcwd))))))
    (with-open-file (in filename)
      (let ((line-no 1))
        (handler-case 
            (loop while t do
              ;;(/ 1 0)
              (let ((line (read-line in t)))
                (cond 
                  ((and (stringp line) (> (length line) 0))
                   (let ((vals (progressive line)))
                     (format t "line (~a)= ~a : [~a]~%" line-no line vals)
                     (setf (aref array line-no) vals)
                     (incf line-no)))
                  (t nil))))
          (end-of-file (c) 
            t
            array
            ;;(format t "condition caught = ~a~%" c)
            )
          (error (c) (format t "some other error caught = ~a~%" c)))))))


;; given a string , progressively read from it 
(defun progressive(s)
  (let ((vals '())
        (s2 s)
        (index 0))
    (handler-case 
        (loop while t do
          (multiple-value-bind (result index2)
              (read-from-string s2 t 'end :start index)
            (setq vals (append vals (list result)))
            (setq index index2)))
      (end-of-file (c) 
        ;;(format t "condition caught = ~a~%" c)
        vals
        ))))


;; how many entries are SAFE 
;; differ by atleast 1 and atmost 3 -> set 1 , 2 or 3 
(defun safe-list(xs)
  (cond
    ((null xs) t)
    ((null (cdr xs)) t)
    (t (let ((a (car xs))
             (b (cadr xs)))
         (cond
           ((> b a) (increasing-safe-list xs))
           ((< b a) (decreasing-safe-list xs))
           (t nil))))))

(defun increasing-safe-list(xs)
  (cond
    ((null xs) t)
    ((null (cdr xs)) t)
    (t (let ((a (car xs))
             (b (cadr xs)))
         (let ((diff (- b a))) 
           (cond
             ((not (> b a)) (values nil (format nil "~a is not less than ~a" a b)))
             ((not (and (>= diff 1) (<= diff 3))) 
              (values nil (format nil "difference between ~a and ~a is not between 1 and 3" a b)))
             (t (increasing-safe-list (cdr xs)))))))))

(defun decreasing-safe-list(xs)
  (cond
    ((null xs) t)
    ((null (cdr xs)) t)
    (t (let ((a (car xs))
             (b (cadr xs)))
         (let ((diff (- a b))) 
           (cond
             ((not (> a b)) (values nil (format nil "~a is not greater than ~a" a b)))
             ((not (and (>= diff 1) (<= diff 3))) 
              (values nil (format nil "difference between ~a and ~a is not between 1 and 3" a b)))
             (t (decreasing-safe-list (cdr xs)))))))))

             
(safe-list '(7 6 4 2 1)) ;; : Safe because the levels are all decreasing by 1 or 2.
(safe-list '(1 2 7 8 9)) ;;: Unsafe because 2 7 is an increase of 5.
(safe-list '(9 7 6 2 1)) ;;: Unsafe because 6 2 is a decrease of 4.
(safe-list '(1 3 2 4 5)) ;;: Unsafe because 1 3 is increasing but 3 2 is decreasing.
(safe-list '(8 6 4 4 1)) ;;: Unsafe because 4 4 is neither an increase or a decrease.
(safe-list '(1 3 6 7 9)) ;;: Safe because the levels are all increasing by 1, 2, or 3.

;; debug we can trace, also step 
;; (trace increasing-safe-list)
;; (trace decreasing-safe-list)
;; (trace safe-list)
;;(untrace)


(defun challenge ()
  (let ((arr (read-values))
        (tot 0))
    (loop for i from 1 to 1000 do 
      (let ((xs (aref arr i)))
        (when (and (not (null xs)) (safe-list xs))
          (format t "line (~a) : [~a] is safe (tot =~a) ~%" i xs tot)
          (incf tot))))
    (format t "total safe lines ~a ~%" tot)
    tot))

;; (challenge) => 421
;; total safe lines 421 

;; =================================================================================
;; part two 
;; ================================================================================
        
#|

thought it meant any number of mistakes allowed but keep values in place ...

;; how many entries are SAFE 
;; differ by atleast 1 and atmost 3 -> set 1 , 2 or 3 
(defun safe-list2(xs)
  (let ((level 0)
        (mistakes '()))
    (cond
      ((null xs) t)
      ((null (cdr xs)) t)
      (t (let ((a (car xs))
               (b (cadr xs)))
           (cond
             ((> b a) (increasing-safe-list2 xs level mistakes))
             ((< b a) (decreasing-safe-list2 xs level mistakes))
             (t nil)))))))


(defun increasing-safe-list2(xs level mistakes)
  (cond
    ((> level 1) (values nil (format nil "too many mistakes ~a : ~a" level mistakes)))
    ((null xs) t)
    ((null (cdr xs)) t)
    (t (let ((a (car xs))
             (b (cadr xs)))
         (let ((diff (- b a))) 
           (cond
             ((not (> b a)) (increasing-safe-list2 (cdr xs) (+ level 1) 
                                                   (append mistakes
                                                           (list (format nil "~a is not less than ~a" a b)))))
             ((not (and (>= diff 1) (<= diff 3)))
              (increasing-safe-list2 (cdr xs) (+ level 1) 
                                     (append mistakes
                                             (list (format nil "difference between ~a and ~a is not between 1 and 3" a b)))))
             (t (increasing-safe-list2 (cdr xs) level mistakes))))))))



(defun decreasing-safe-list2(xs level mistakes)
  (cond
    ((> level 1) (values nil (format nil "too many mistakes ~a : ~a" level mistakes)))
    ((null xs) t)
    ((null (cdr xs)) t)
    (t (let ((a (car xs))
             (b (cadr xs)))
         (let ((diff (- a b))) 
           (cond             
             ((not (> a b)) (decreasing-safe-list2 (cdr xs) (+ level 1) 
                                                   (append mistakes
                                                           (list (format nil "~a is not greater than ~a" a b)))))
             ((not (and (>= diff 1) (<= diff 3))) 
              (increasing-safe-list2 (cdr xs) (+ level 1) 
                                     (append mistakes
                                             (list (format nil "difference between ~a and ~a is not between 1 and 3" a b)))))
             (t (decreasing-safe-list2 (cdr xs) level mistakes))))))))


|#

;; completely wrong approach 
;;
;; can remove one value from the list to get a safe list ?
;; remove the nth item from a list , want to keep all subset lists created
;; (1 2 3 4 5 6 )
;;    2 3 4 5 6     ... sublist 1
;;  1   3 4 5 6
;;  1 2   4 5 6
;;  1 2 3   5 6
;;  1 2 3 4 5       ... sublist 5
(defun subsets(xs) 
  (let ((before '())
        (lists '()))        
    (loop while (not (null xs)) do
           (let* ((after (cdr xs))
                  (tmp (append before after)))
             (when (safe-list tmp)
               (throw 'found-safe t))
             ;;(subsets-debug before after)
             (setq lists (cons tmp lists))
             (setq before (append before (list (car xs))))
             (setq xs (cdr xs))))))

;;(defun subsets-debug (before after) t)

;';  (format t "before (~a) after (~a)" before after))
;;(trace subsets-debug)
;;(untrace)      

(defun safe-list2 (xs)
  (or (safe-list xs)    
      (catch 'found-safe 
        (subsets xs)
        nil)))


(safe-list2 '(7 6 4 2 1)) ;; Safe without removing any level.
(safe-list2 '(1 2 7 8 9)) ;; Unsafe regardless of which level is removed.
(safe-list2 '(9 7 6 2 1)) ;; Unsafe regardless of which level is removed.
(safe-list2 '(1 3 2 4 5)) ;; Safe by removing the second level, 3.
(safe-list2 '(8 6 4 4 1)) ;; Safe by removing the third level, 4.
(safe-list2 '(1 3 6 7 9)) ;; Safe without removing any level.


(defun challenge2 ()
  (let ((arr (read-values))
        (tot 0))
    (loop for i from 1 to 1000 do 
      (let ((xs (aref arr i)))
        (when (and (not (null xs)) (safe-list2 xs))
          (format t "line (~a) : [~a] is safe-list2 (tot =~a) ~%" i xs tot)
          (incf tot))))
    (format t "total safe lines ~a ~%" tot)
    tot))

          
;; (challenge2)
;;total safe lines 476 










