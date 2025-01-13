

(defpackage #:aoc
  (:use #:cl))

(in-package #:aoc)

;; enable full debugging capabilties
(declaim (optimize (speed 0) (space 0) (debug 3)))


;; 
(defparameter *data* 
  (let ((filename (merge-pathnames "input.txt" 
                                   (truename (merge-pathnames "../../" (uiop:getcwd)))))
        (seq '()))
    (with-open-file (in filename)
      (handler-case 
          (loop while t do
            ;;(/ 1 0)
            (let ((s (read-line in)))
              (format t "got ~a~%" s)
              (setq seq (cons (format nil "~a" s) seq))))
        (end-of-file (c) 
          t
          (setq seq (reverse seq))
          ;; post processing each line of data 
          (aggregate seq)
          ;; (let ((result (make-array (length seq))))
          ;;   (loop for i from 0 to (- (length seq) 1) do
          ;;     (setf (aref result i) (nth i seq)))
          ;;   result)
          ;;(format t "condition caught = ~a~%" c)
          )
        (error (c) (format t "some other error caught = ~a~%" c))))))


;; pattern match XXX | YYY
;; (split-sequence:split-sequence #\- "--a")
;; (split-sequence:split-sequence  "|" "47|53")
;; (cl-ppcre:split #\| "47|53")
;;
;; (mapcar #'(lambda (s)(read-from-string s)) (cl-ppcre:split #\| "47|53"))
;; (mapcar #'(lambda (s)(read-from-string s)) (cl-ppcre:split #\, "75,47,61,53,29"))
;;
(defun string-contains-character(s c)
  (catch 'found
    (loop for i from 0 to (- (length s) 1) do
      (when (char= (char s i) c)
        (throw 'found t)))
    nil))


(defun string-contains-bar(s)
  (string-contains-character s #\|))

(defun string-contains-comma(s)
  (string-contains-character s #\,))


(defun aggregate (lines)
  (let ((table-1 '())
        (table-2 '()))
    (loop for line in lines do 
      (cond
        ((equalp line "") nil)
        ((string-contains-bar line) 
         (let ((pair-nums (mapcar #'(lambda (s)(read-from-string s)) (cl-ppcre:split #\| line))))
           (setq table-1 (cons pair-nums table-1))))
        ((string-contains-comma line) 
         (let ((nums (mapcar #'(lambda (s)(read-from-string s)) (cl-ppcre:split #\, line))))
           (setq table-2 (cons nums table-2))))
        (t (error (format nil "aggregate : cannot decode ~a" line)))))
    `('table-1 ,(reverse table-1) 'table-2  ,(reverse table-2))))


(defun test ()
  (let ((*data* "
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"))
    (setq *data* (cl-ppcre:split #\linefeed *data*))
    (setq *data* (aggregate *data*))
    (figure-out)))

(defun part-1 ()
  (figure-out))



(defun middle-value (xs)
  (let ((len (length xs)))
    (nth (floor (/ len 2)) xs)))

(defun forward-pairs-of(xs)
  (let ((len (length xs))
        (pairs '()))
    (loop for i from 0 to (- len 1) do
      (loop for j from (+ i 1) to (- len 1) do
        (when (< i j)
          (setq pairs (cons (list (nth i xs) (nth j xs)) pairs)))))
    (reverse pairs)))


(defun figure-out ()
  (let ((known-pairs (nth 1 *data*))
        (updates (nth 3 *data*))
        (satisfied '())
        (un-satisfied '())
        (mid-count 0))
    (loop for update in updates do 
      (catch 'unsatisfy
      (let ((fwd (forward-pairs-of update))
            (mid (middle-value update)))
      (format t "update ~a : (middle ~a) : ~%(forward-pairs ~a)~%" 
              update 
              mid
              fwd)     
        
        ;; checking fp against kp 
        (loop for fp in fwd do
          (catch 'satisfy 
          (loop for kp in known-pairs do
            (let ((fp1 (car fp))(fp2 (cadr fp))
                  (kp1 (car kp))(kp2 (cadr kp)))
              ;;(format t "check fp (~a  ~a) : kp (~a  ~a) ~%" fp1 fp2 kp1 kp2)
              (when (and (= fp1 kp1) (= fp2 kp2)) 
                ;;;(format t " satisfied by ~a ~%" kp)
                (throw 'satisfy t))
              (when (and (= fp1 kp2) (= fp2 kp1)) 
                ;;(format t " conflicts ~a with ~a ~%" fp kp)
                (format t "~%***UN-SATISFIED ~a ~%~%" update)
                (setq un-satisfied (cons update un-satisfied))
                (throw 'unsatisfy nil))
              ))))
        ;; satisfied 
        (setq satisfied (cons update satisfied))              
        (incf mid-count mid)
        (format t "~%***SATISFIED ~a : mid (~a) : tot mid (~a) ~%~%" update mid mid-count)
        )))
    (format t "~%***ALL SATISFIED ~a ~%" (reverse satisfied))
    (format t "~%***ALL UN-SATISFIED ~a ~%" (reverse un-satisfied))
    (format t "~%***TOT MID POINT COUNT ~a ~%" mid-count)
    mid-count))


;; (part-1) => 5064













      









