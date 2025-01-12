

(defpackage #:aoc
  (:use #:cl))

(in-package #:aoc)

(declaim (optimize (speed 0) (space 0) (debug 3)))


;; 
(defparameter *chars* 
  (let ((filename (merge-pathnames "input.txt" 
                                   (truename (merge-pathnames "../" (uiop:getcwd)))))
        (chars '()))
    (with-open-file (in filename)
      (handler-case 
          (loop while t do
            ;;(/ 1 0)
            (let ((ch (read-char in)))
              (setq chars (cons ch chars))))
        (end-of-file (c) 
          t
          (setq chars (make-array (length chars) :initial-contents (reverse chars)))
          (setq chars (coerce chars 'string))
          chars
          ;;(format t "condition caught = ~a~%" c)
          )
        (error (c) (format t "some other error caught = ~a~%" c))))))




;;(trace achar=)
;;(untrace)

#|
test cases 

AOC> (achar= #\a  #(#\a #\b) 0)
T
AOC> (achar= #\b  #(#\a #\b) 1)
T
AOC> (achar= #\c  #(#\a #\b) 1)
NIL
AOC> (achar= #\a  #(#\a #\b) 1)
NIL
AOC> (achar= #\z  #(#\a #\b) 1)
NIL
AOC> (anumc    #(#\0 #\1) 1)
(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
AOC> (anumc    #(#\0 #\1) 0)
(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
AOC> (anumc    #(#\0 #\1 #\a #\b #\c ) 2)
NIL
AOC> (anumc    #(#\0 #\1 #\a #\b #\c ) 3)
NIL
AOC> (anumc    #(#\0 #\1 #\a #\b #\c ) 4)
NIL
|#


;; m u l ( X X X , Y Y Y ) -> X * Y 

;;  m u l       (     X   ,   Y       )
;;  m u l       (     X   ,   Y Y     )
;;  m u l       (     X   ,   Y Y Y   )
;;  m u l       (     X X  ,   Y      )
;;  m u l       (     X X  ,   Y Y     )
;;  m u l       (     X X  ,   Y Y Y   )
;;  m u l       (     X X X ,   Y     )
;;  m u l       (     X X X ,   Y Y   )
;;  m u l       (     X X X ,   Y Y Y  )    12 chars if match
;;                4     3    1     3     1   
  
(defun anumc (s i)
  (member (aref s i) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(defun c2n (s i)
  (let ((ch (aref s i)))
    (case ch        
      (#\0 0)
      (#\1 1)
      (#\2 2)
      (#\3 3)
      (#\4 4)
      (#\5 5)
      (#\6 6)
      (#\7 7)
      (#\8 8)
      (#\9 9)
      (otherwise (error "expected digit character")))))

(defun clist2n (s i r)
  (cond
    ((= r 1) (c2n s i))
    ((= r 2) (+ (* 10 (c2n s i)) (c2n s (+ i 1))))
    ((= r 3) (+ (* 100 (c2n s i)) (* 10 (c2n s (+ i 1))) (c2n s (+ i 2))))
    (t (error "clist2n "))))




(defun report (i a b)
  (format t "REP ~a ~a ~a ~%" i a b))

;;; array chars 
(defun achar= (ch s i)
  (char= ch (char s i)))

;; cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         num num      len  count
;;          X  Ys
;; X Y      1 1          2     1
;; X YY     1 2          3     2
;; X YYY    1 3          4     3
;;
;; XX Y     2 1          3     4
;; XX YY    2 2          4     5
;; XX YYY   2 3          5     6
;;
;; XXX Y    3 1          4     7
;; XXX YY   3 2          5     8 
;; XXX YYY  3 3          6     9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; X Y
(defmacro case-1-1 ()
  `(when 
       (and (achar= #\m s (+ i 0)) ;; X Y
            (achar= #\u s (+ i 1))
            (achar= #\l s (+ i 2))
            (achar= #\( s (+ i 3))
            (anumc      s (+ i 4))
            (achar= #\, s (+ i 5))
            (anumc      s (+ i 6))
            (achar= #\) s (+ i 7)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (let ((x (clist2n s (+ i 4) 1))
           (y  (clist2n s (+ i 6) 1)))
     (format t "matched 1x1 case (~a) :  ~A :  ~a  x  ~a  ~%" (subseq s i (+ i 8)) i x y )
       (incf tot (* x y))
     (setq i (+ i 7))
     (continue))))


(defmacro case-1-2 ()
  `(when 
       (and (achar= #\m s (+ i 0)) ;; X Y
            (achar= #\u s (+ i 1))
            (achar= #\l s (+ i 2))
            (achar= #\( s (+ i 3))
            (anumc      s (+ i 4))
            (achar= #\, s (+ i 5))
            (anumc      s (+ i 6))
            (anumc      s (+ i 7))
            (achar= #\) s (+ i 8)))
     (let ((x (clist2n s (+ i 4) 1)) 
           (y (clist2n s (+ i 6) 2)))     
     (format t "poossible ~A :  ~a  x  ~a  ~%" i x y)
       (incf tot (* x y))
     (setq i (+ i 8))
     (continue))))



(defmacro case-1-3 ()
  `(when 
       (and (achar= #\m s (+ i 0)) ;; X Y
            (achar= #\u s (+ i 1))
            (achar= #\l s (+ i 2))
            (achar= #\( s (+ i 3))
            (anumc      s (+ i 4))
            (achar= #\, s (+ i 5))
            (anumc      s (+ i 6))
            (anumc      s (+ i 7))
            (anumc      s (+ i 8))
            (achar= #\) s (+ i 9)))
     (let ((x (clist2n s (+ i 4) 1))
           (y   (clist2n s (+ i 6) 3)))
     (format t "poossible ~A :  ~a  x  ~a  ~%" i x y)
       (incf tot (* x y))
     (setq i (+ i 9))
     (continue))))



;; XX Y
(defmacro case-2-1 ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) 
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))

        (achar= #\, s (+ i 6))

        (anumc      s (+ i 7))
        
        (achar= #\) s (+ i 8)))
     (let ((x (clist2n s (+ i 4) 2)) 
           (y  (clist2n s (+ i 7) 1)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (incf tot (* x y))
     (setq i (+ i 8))
     (continue))))






;; XX YY
(defmacro case-2-2 ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))

        (achar= #\, s (+ i 6))

        (anumc      s (+ i 7))
        (anumc      s (+ i 8))
        
        (achar= #\) s (+ i 9)))
     (let ((x (clist2n s (+ i 4) 2))
           (y   (clist2n s (+ i 7) 2)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (incf tot (* x y))
     (setq i (+ i 9))
     (continue))))



;; XX YYY
(defmacro case-2-3 ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))

        (achar= #\, s (+ i 6))

        (anumc      s (+ i 7))
        (anumc      s (+ i 8))
        (anumc      s (+ i 9))

        (achar= #\) s (+ i 10)))
     (let ((x (clist2n s (+ i 4) 2))
           (y   (clist2n s (+ i 7) 3)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (incf tot (* x y))
     (setq i (+ i 10))
     (continue))))


;; XXX Y
(defmacro case-3-1 ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))
        (anumc      s (+ i 6))

        (achar= #\, s (+ i 7))

        (anumc      s (+ i 8))

        (achar= #\) s (+ i 9)))
     (let ((x (clist2n s (+ i 4) 3))
           (y   (clist2n s (+ i 8) 1)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (incf tot (* x y))
     (setq i (+ i 9))
     (continue))))


;; XXX YY
(defmacro case-3-2 ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))
        (anumc      s (+ i 6))

        (achar= #\, s (+ i 7))

        (anumc      s (+ i 8))
        (anumc      s (+ i 9))

        (achar= #\) s (+ i 10)))
     (let ((x (clist2n s (+ i 4) 3))
           (y   (clist2n s (+ i 8) 2)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (incf tot (* x y))
     (setq i (+ i 10))
     (continue))))


;;  XXX  YYYY
(defmacro case-3-3 ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))
        (anumc      s (+ i 6))

        (achar= #\, s (+ i 7))

        (anumc      s (+ i 8))
        (anumc      s (+ i 9))
        (anumc      s (+ i 10))

        (achar= #\) s (+ i 11)))
     (let ((x (clist2n s (+ i 4) 3))
           (y   (clist2n s (+ i 8) 3)))
     (format t "matched 3x3 case (~a) : ~A :  ~a  x  ~a  ~%" (subseq s i (+ i 13)) i x y)
       (incf tot (* x y))
     (setq i (+ i 11))
     (continue))))




;; in a string find mul pattern at some index i      
(defun find-mul (s)
  (let ((slen (- (length s) 1))
        (tot 0))
    (loop for i from 0 to slen do
      ;;(format t "looking at string => ~a~%" (subseq s i slen))

      (case-1-1)
      (case-1-2)

      (case-2-1)
      (case-2-2)

      (case-1-3)
      (case-3-1)

      (case-2-3)
      (case-3-2)

      (case-3-3)
      )
    tot))

(defun test ()
  (find-mul "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"))

;; poossible 1 :  2  x  4  
;; poossible 29 :  5  x  5  
;; p[ooossible 53 :  11  x  8  
;; 121

;;(find-mul *chars*)
;;=> 175015740


;;========================================================================================
;; part two 
;; 

;;  do() 
(defmacro case-do ()
  `(when 
       (and
        (achar= #\d s (+ i 0)) ;; XXX YYY
        (achar= #\o s (+ i 1))
        (achar= #\( s (+ i 2))
        (achar= #\) s (+ i 3)))
     (setq enable t)
     (setq i (+ i 3))
     (continue)))



;;   don't()
(defmacro case-dont ()
  `(when 
       (and
        (achar= #\d s (+ i 0)) ;; XXX YYY
        (achar= #\o s (+ i 1))
        (achar= #\n s (+ i 2))
        (achar= #\' s (+ i 3))
        (achar= #\t s (+ i 4))        
        (achar= #\( s (+ i 5))
        (achar= #\) s (+ i 6)))
     (setq enable nil)
     (setq i (+ i 6))
     (continue)))


;; cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         num num      len  count
;;          X  Ys
;; X Y      1 1          2     1
;; X YY     1 2          3     2
;; X YYY    1 3          4     3
;;
;; XX Y     2 1          3     4
;; XX YY    2 2          4     5
;; XX YYY   2 3          5     6
;;
;; XXX Y    3 1          4     7
;; XXX YY   3 2          5     8 
;; XXX YYY  3 3          6     9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; X Y
(defmacro case-1-1-b ()
  `(when 
       (and (achar= #\m s (+ i 0)) ;; X Y
            (achar= #\u s (+ i 1))
            (achar= #\l s (+ i 2))
            (achar= #\( s (+ i 3))
            (anumc      s (+ i 4))
            (achar= #\, s (+ i 5))
            (anumc      s (+ i 6))
            (achar= #\) s (+ i 7)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (let ((x (clist2n s (+ i 4) 1))
           (y  (clist2n s (+ i 6) 1)))
     (format t "matched 1x1 case (~a) :  ~A :  ~a  x  ~a  ~%" (subseq s i (+ i 8)) i x y )
       (when enable (incf tot (* x y)))
     (setq i (+ i 7))
     (continue))))


(defmacro case-1-2-b ()
  `(when 
       (and (achar= #\m s (+ i 0)) ;; X Y
            (achar= #\u s (+ i 1))
            (achar= #\l s (+ i 2))
            (achar= #\( s (+ i 3))
            (anumc      s (+ i 4))
            (achar= #\, s (+ i 5))
            (anumc      s (+ i 6))
            (anumc      s (+ i 7))
            (achar= #\) s (+ i 8)))
     (let ((x (clist2n s (+ i 4) 1)) 
           (y (clist2n s (+ i 6) 2)))     
     (format t "poossible ~A :  ~a  x  ~a  ~%" i x y)
       (when enable (incf tot (* x y)))
     (setq i (+ i 8))
     (continue))))



(defmacro case-1-3-b ()
  `(when 
       (and (achar= #\m s (+ i 0)) ;; X Y
            (achar= #\u s (+ i 1))
            (achar= #\l s (+ i 2))
            (achar= #\( s (+ i 3))
            (anumc      s (+ i 4))
            (achar= #\, s (+ i 5))
            (anumc      s (+ i 6))
            (anumc      s (+ i 7))
            (anumc      s (+ i 8))
            (achar= #\) s (+ i 9)))
     (let ((x (clist2n s (+ i 4) 1))
           (y   (clist2n s (+ i 6) 3)))
     (format t "poossible ~A :  ~a  x  ~a  ~%" i x y)
       (when enable (incf tot (* x y)))
     (setq i (+ i 9))
     (continue))))



;; XX Y
(defmacro case-2-1-b ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) 
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))

        (achar= #\, s (+ i 6))

        (anumc      s (+ i 7))
        
        (achar= #\) s (+ i 8)))
     (let ((x (clist2n s (+ i 4) 2)) 
           (y  (clist2n s (+ i 7) 1)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (when enable (incf tot (* x y)))
     (setq i (+ i 8))
     (continue))))






;; XX YY
(defmacro case-2-2-b ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))

        (achar= #\, s (+ i 6))

        (anumc      s (+ i 7))
        (anumc      s (+ i 8))
        
        (achar= #\) s (+ i 9)))
     (let ((x (clist2n s (+ i 4) 2))
           (y   (clist2n s (+ i 7) 2)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (when enable (incf tot (* x y)))
     (setq i (+ i 9))
     (continue))))



;; XX YYY
(defmacro case-2-3-b ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))

        (achar= #\, s (+ i 6))

        (anumc      s (+ i 7))
        (anumc      s (+ i 8))
        (anumc      s (+ i 9))

        (achar= #\) s (+ i 10)))
     (let ((x (clist2n s (+ i 4) 2))
           (y   (clist2n s (+ i 7) 3)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (when enable (incf tot (* x y)))
     (setq i (+ i 10))
     (continue))))


;; XXX Y
(defmacro case-3-1-b ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))
        (anumc      s (+ i 6))

        (achar= #\, s (+ i 7))

        (anumc      s (+ i 8))

        (achar= #\) s (+ i 9)))
     (let ((x (clist2n s (+ i 4) 3))
           (y   (clist2n s (+ i 8) 1)))
     ;;(format t "REP ~a ~a ~a ~%" i (c2n s (+ i 4)) (c2n s (+ i 6)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (when enable (incf tot (* x y)))
     (setq i (+ i 9))
     (continue))))


;; XXX YY
(defmacro case-3-2-b ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))
        (anumc      s (+ i 6))

        (achar= #\, s (+ i 7))

        (anumc      s (+ i 8))
        (anumc      s (+ i 9))

        (achar= #\) s (+ i 10)))
     (let ((x (clist2n s (+ i 4) 3))
           (y   (clist2n s (+ i 8) 2)))
     (format t "p[ooossible ~A :  ~a  x  ~a  ~%" i x y)
       (when enable (incf tot (* x y)))
     (setq i (+ i 10))
     (continue))))


;;  XXX  YYYY
(defmacro case-3-3-b ()
  `(when 
       (and
        (achar= #\m s (+ i 0)) ;; XXX YYY
        (achar= #\u s (+ i 1))
        (achar= #\l s (+ i 2))
        (achar= #\( s (+ i 3))

        (anumc      s (+ i 4))
        (anumc      s (+ i 5))
        (anumc      s (+ i 6))

        (achar= #\, s (+ i 7))

        (anumc      s (+ i 8))
        (anumc      s (+ i 9))
        (anumc      s (+ i 10))

        (achar= #\) s (+ i 11)))
     (let ((x (clist2n s (+ i 4) 3))
           (y   (clist2n s (+ i 8) 3)))
     (format t "matched 3x3 case (~a) : ~A :  ~a  x  ~a  ~%" (subseq s i (+ i 13)) i x y)
       (when enable (incf tot (* x y)))
     (setq i (+ i 11))
     (continue))))




;; in a string find mul pattern at some index i      
(defun find-mul2 (s)
  (let ((slen (- (length s) 1))
        (tot 0)
        (enable t))
    (loop for i from 0 to slen do
      ;;(format t "looking at string => ~a~%" (subseq s i slen))
      (case-do)
      (case-dont)

      (case-1-1-b)
      (case-1-2-b)

      (case-2-1-b)
      (case-2-2-b)

      (case-1-3-b)
      (case-3-1-b)

      (case-2-3-b)
      (case-3-2-b)

      (case-3-3-b)
      )
    tot))

(defun test2 ()
  (find-mul2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))

(defun part2 ()
  (find-mul2 *chars*))

;; (part1)
;;=> 112272912



