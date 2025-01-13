

(defpackage #:aoc
  (:use #:cl))

(in-package #:aoc)

;; enable full debugging capabilties
 (declaim (optimize (speed 0) (space 0) (debug 3)))
;;(declaim (optimize (speed 3) (space 0) (debug 0)))


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


;; split into lines
;; if line contains a bar character | split string based on that
;;   read split string as lisp code - should generate numbers
;; if line contains a comma character , split string based on that
;;   read split string as lisp code - should generate numbers
;; if line is empty - ignore
;; should create two tables as result
;; table 1 - all the XXX | YYY pairs
;; table 2 - all the updates (A B C D ...) (A B C) (A B C D E ..)
;;
;; *example*  small example table given in problem description
;; *data*   big tables from files
;; 
(defparameter *example* 
  (aggregate 
   (cl-ppcre:split #\linefeed 
"
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
")))



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




(defun test ()
  (let ((*data* *example*))
    ;; (setq *data* (cl-ppcre:split #\linefeed *data*))
    ;; (setq *data* (aggregate *data*))
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

;; ===================================================================================
;; part two 
;;
;; usign *known-pairs* as a dynamically scoped varialbe
;; trying to be too clever me things , 
;; where is the slow down ? 
;; how profile this code ??
;;


(defparameter *known-pairs* '())
(defparameter *known-pairs-hash* '())


(defmacro per-update ()
    ;; ---- per update
    `(loop for update in updates do 
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
          (loop for kp in *known-pairs* do
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
        ))))
    ;; -------- per update


;; ;; we tried shuffling ... but took too long
;; ***SATISFIED ORIG (56 96 45 41 49 72 36 73 68 42 29 62 34 16 33 69 66 32 77) 
;; : SHUFFLED (73 66 72 45 36 56 34 96 62 69 16 41 32 29 77 49 42 68 33) : 
;; mid (69) : tot mid (69) 
;; 
;; ***SATISFIED ORIG (33 53 78 14 77 82 32 49 39) 
;; : SHUFFLED (32 39 77 49 33 82 78 53 14) : 
;; mid (33) : tot mid (102) 
;; 
;; ***SATISFIED ORIG (29 72 36 68 39 57 49 82 62 41 33 77 92) 
;; : SHUFFLED (72 36 62 41 39 29 57 77 49 68 33 92 82) : 
;; mid (57) : tot mid (159) 
;; 
;; ***SATISFIED ORIG (37 75 61 65 31 47 87) 
;; : SHUFFLED (65 47 37 75 61 87 31) : 
;; mid (75) : tot mid (234) 

     
;;  hash from pairs 
;; two way hash table 
;; PR1 PR2 
;; PR1 -> indexes to HASH 2
;;        PR2 -> indexes to either forward , backward or nil
(defun hash-from-pairs (xs) 
  (let ((hash (make-hash-table)))
    (dolist (pr xs)
      (let ((pr1 (car pr))
            (pr2 (cadr pr)))
        (format t  "hfp pairs ~a ~a  ~%" pr1 pr2)

        ;; PR1 -> PR2 -> forward  
        (let ((inner-hash (gethash pr1 hash nil)))
          (cond
            ;; setup inner hash if does not exist
            ((null inner-hash) (let ((inner-hash (make-hash-table)))
                                 (setf (gethash pr2 inner-hash) 'after)
                                 (setf (gethash pr1 hash) inner-hash)))
            (t (setf (gethash pr2 inner-hash) 'after))))

        ;; PR2 -> PR1 -> backward 
        (let ((inner-hash (gethash pr2 hash nil)))
          (cond
            ;; setup inner hash if does not exist
            ((null inner-hash) (let ((inner-hash (make-hash-table)))
                                 (setf (gethash pr1 inner-hash) 'before)
                                 (setf (gethash pr2 hash) inner-hash)))
            (t (setf (gethash pr1 inner-hash) 'before))))
        ))        
    hash))



(defun has-direction(x y)
  (catch 'result 
    ;; try forward direction
    (let ((inner-hash (gethash x *known-pairs-hash* nil)))
      (cond
        ((null inner-hash) nil)
        (t (let ((z (gethash y inner-hash nil)))
             (cond
               ((null z) nil)
               (t (throw 'result z)))))))
    ;; try backward direction
    (let ((inner-hash (gethash y *known-pairs-hash* nil)))
      (cond
        ((null inner-hash) nil)
        (t (let ((z (gethash x inner-hash nil)))
             (cond
               ((null z) nil)
               (t (throw 'result z)))))))
    nil))




(defmacro per-unsatisfied-update ()
  ;; ---- per update
  `(progn
     (setq mid-count 0)
     
     (loop for update in un-satisfied do 
       (let ((final (catch 'completely-satisfied
                      (fast-shuffle update)))) ;; fast-shuffle 
         (cond
           ((listp final) (let ((mid (middle-value final)))
                            (incf mid-count mid)))
           (t (error "could not satify update ~a~%" update)))
       (format t " update ~a : final ~a ~%" update final)))))




#|
fast shuffle :
xs some list of numbers that do not satisfy constraints
update empty
take an x 

before         after
 ()           (a b c)
           u < a then (u a b c) surely ?


update is result 

first item in update is always in right place
just need to figure out where everything else goes

linear constraints ??
|#
(defun fast-shuffle (xs)
  (let* ((update (list (car xs)))
         (update2 update))
    (dolist (x (cdr xs))
      (block next
        (let ((before '())
              (after update2))
          (setq update update2)
          ;; 
          (let ((ok (satisfiable-p2 update)))
            (cond
              ((not ok) 
               ;;(format t "failed to make proper update ? from: ~a  made:~a ~%"  xs update)
               (throw 'banana 'monkey))
              (t 
               ;;(format t "so far from: ~a  made:~a ~%"  xs update)
               t
                 )))

          (dolist (u update)
            (let ((dir (has-direction x u)))
              ;; spill direction
              ;;(when dir (format t "direction ~a is ~a the ~a ~%" x dir u))
              (cond
                ((and (eq dir 'after) (null (cdr after))) ;; no more to check
                 (setq update2 (append (reverse before) after (list x)))
                 (return-from next))                
                ((or (null dir) (eq dir 'after))
                 (setq before (cons u before))
                 (setq after (cdr after)))
                ((eq dir 'before) ;; x then u 
                 (setq update2 (append (reverse before) (list x) after))
                 (return-from next))
                (t (error "wtf")))))
          )))
    (cond 
      ((and (= (length xs) (length update2))  (satisfiable-p2 update2))
       (format t "~%~%")
       update2)
      (t (error "failed fast-shuffle")))))




(defun figure-out2 ()
  (let ((*known-pairs* (nth 1 *data*))
        (updates (nth 3 *data*))
        (satisfied '())
        (un-satisfied '())
        (completely-satisfied '())
        (mid-count 0))
    (let ((*known-pairs-hash* (hash-from-pairs *known-pairs*)))
      ;; find all satisifed / un-satisfied
      (per-update)

      (loop for unsat in un-satisfied do
        (format t "~%local un-satisfied ~a ~%" unsat))
      
      (format t "~%~%~%")

      ;; for each un-satisfied 
      (per-unsatisfied-update)
      
      (format t "~%***COMPLETELY SATISFIED ~a ~%" (reverse completely-satisfied))
      (format t "~%***TOT MID POINT COUNT ~a ~%" mid-count)
      mid-count)))



(defun satisfiable-p (update)
  (let ((fwd (forward-pairs-of update)))
    ;; (format t "update ~a : (middle ~a) : ~%(forward-pairs ~a)~%" 
    ;;         update 
    ;;         mid
    ;;         fwd)     
    
    (catch 'unsatisfiable 
    ;; checking fp against kp 
    (loop for fp in fwd do
      (catch 'satisfy 
        (loop for kp in *known-pairs* do
          (let ((fp1 (car fp))(fp2 (cadr fp))
                (kp1 (car kp))(kp2 (cadr kp)))
            ;;(format t "check fp (~a  ~a) : kp (~a  ~a) ~%" fp1 fp2 kp1 kp2)
            (when (and (= fp1 kp1) (= fp2 kp2)) 
                ;;;(format t " satisfied by ~a ~%" kp)
              (throw 'satisfy t))
            (when (and (= fp1 kp2) (= fp2 kp1)) 
              ;;(format t " conflicts ~a with ~a ~%" fp kp)              
              (throw 'unsatisfiable nil))
            ))))

    ;; satisfied 
    t
    )))


(defun satisfiable-p2 (update)
  (let ((fwd (forward-pairs-of update)))    
    (catch 'unsatisfiable 
    ;; checking fp against kp 
    (loop for fp in fwd do
      (let ((fp1 (car fp))(fp2 (cadr fp)))            
        (let ((dir (has-direction fp1 fp2)))
          (cond
            ((eq dir 'forward) t)
            ((eq dir 'backward) (throw 'unsatisfiable nil))
            (t nil)))))
    t)))



;; shuffle 
;; choice to take item from pool or not 
(defun shuffle (xs)
  (let ((updates '()))
    (labels ((shuffle-helper (pool known)
               ;; recurse otherwise
               (cond
                 ((not (satisfiable-p2 known)) nil)               
                 ((null pool) 
                  ;; we are done now - only need 1 solution
                  (throw 'completely-satisfied known)
                  ;;(format t "known ~a ~%" known)
                  ;;(setq updates (cons known updates))
                  )                 
                 (t (loop for p in pool do
                   (shuffle-helper (remove p pool :count 1) (cons p known)))))))
      (shuffle-helper xs '())
      updates)))




(defun test2 ()
  (let ((*data* *example*))
    (figure-out2)))

(defun part-2 ()
  (figure-out2))

#|
 (part-2) => 5152

key to solving this problem was making a fast-shuffle 
   where we know constraints 
     a , b  either a must come before b
                   a must come after b
                   we dont care - can be before or after


AOC> (time (part-2))  
***TOT MID POINT COUNT 5152 
Evaluation took:
  1.468 seconds of real time
  0.167881 seconds of total run time (0.159812 user, 0.008069 system)
  [ Run times consist of 0.002 seconds GC time, and 0.166 seconds non-GC time. ]
  11.44% CPU
  5,410,202,192 processor cycles
  54,937,312 bytes consed
  
5152
most of that is printing to screen whats going on

|#











      









