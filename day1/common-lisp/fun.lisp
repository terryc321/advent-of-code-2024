
(defpackage #:aoc
  (:use #:cl))

(in-package #:aoc)


(defun split-list (input)
  (let ((ct 0)
        (alpha '())
        (beta '()))
    (loop for x in input do
      (cond 
        ((zerop (mod ct 2))
         (setq alpha (append alpha (list x))))
        (t 
         (setq beta (append beta (list x)))))
      (incf ct))
    ;; (format t "alphas(~a) = ~a ~%" (length alpha) alpha)
    ;; (format t "betas(~a) = ~a~%" (length beta) beta)
    (values alpha beta)))


#|
open a file and read in numbers from a file
until end of file
this is just painful pain painFUL 
|# 
(defun read-values ()
  (let ((filename #P"/home/terry/code/advent-of-code/advent-of-code-2024/day1/input.txt"))
    (let ((in (open filename :direction :input :if-does-not-exist nil))
          (still-reading t)
          (nums '()))
      (when in 
      (catch 'error 
        (loop while still-reading do 
          (let ((num (read in nil)))
            (cond
              ((not (integerp num))
               (setq still-reading nil))
              (t (setq nums (append nums (list num))))))))      
      (close in)
      nums))))

(defparameter *test-values* 
'(3   4
4   3
2   5
1   3
3   9
3   3))


;; can we recreate the input to check we read values in correctly

;; proceed if all okay
(defun churn (input)
  (multiple-value-bind (alpha beta) (split-list input)    
    (let ((sorted-alpha (sort alpha #'<))
          (sorted-beta (sort beta #'<))
          (total-dist 0))
      (loop while (not (null sorted-alpha)) do
       (let ((a (car sorted-alpha))
             (b (car sorted-beta)))
         (let ((diff (abs (- a b))))
           (setq total-dist (+ total-dist diff))
           t
           (setq sorted-alpha (remove a sorted-alpha :count 1))
           (setq sorted-beta (remove b sorted-beta :count 1)))))
      total-dist)))


;; (churn (read-values)) 
;; 1765812

(defparameter *t* (make-hash-table))
(gethash 1 *t*)
(setf (gethash 2 *t*) 32)
(gethash 2 *t*)


(defun zero-or-count (n h)
    (multiple-value-bind (count has) (gethash n h)
      (cond
        (has count)
        (t 0))))

(defun churn2 (input)
  (multiple-value-bind (alpha beta) (split-list input)    
    (let ((my-hash (make-hash-table))
          (sorted-alpha (sort alpha #'<))
          (sorted-beta (sort beta #'<))
          (total-sum 0))          
      ;; populate my-hash with occurrences in 2nd list 
      (loop for n in sorted-beta do
        (multiple-value-bind (count has) (gethash n my-hash)
          (when (not has)            
            (let ((count 0))
              (loop for m in sorted-beta do 
                (when (= n m) (incf count)))
              (setf (gethash n my-hash) count)))))
      (loop for n in sorted-alpha do 
        (incf total-sum (* n (zero-or-count n my-hash))))
      total-sum)))


;;AOC> (churn2 *test-values*)
;;31
;;AOC> (churn2 (read-values))
;;20520794

















