

;; grid.scm
;; test-grid.scm


;; make me a grid abstraction from 
(import (chicken format))
(import test)

(define fmt format)

;; how many stones after 25 blinks ?
;; Rules
;; 
;; If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
;; 
;; If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. 
;; The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. 
;; (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
;; 
;; If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
;; 
;; Puzzle input : 
;;  [ 30 71441 3784 580926 2 8122942 0 291 ]

(define puzzle '(30 71441 3784 580926 2 8122942 0 291))

(define (blink xs)
  (cond
   ((null? xs) xs)
   (#t (let ((num (car xs))
             (rest (cdr xs)))
         (cond
          ((= num 0) (cons 1 (blink rest)))
          ((even-num-digits? num)
           (cons (left-half num) (cons (right-half num) (blink rest))))
          (#t (cons (* num 2024) (blink rest))))))))


(define (even-num-digits? n)
  (even? (string-length (fmt #f "~a" n))))

(define (left-half n)  (string->number (left-half-str (fmt #f "~a" n))))

(define (right-half n) (string->number (right-half-str (fmt #f "~a" n))))

(define (left-half-str str)   
  (let* ((len (string-length str)))
    (assert (even? len))
    (let ((left (substring str 0 (/ len 2))))
      left)))

(define (right-half-str str)   
  (let* ((len (string-length str)))
    (assert (even? len))
    (let ((right (substring str (/ len 2) len)))
      right)))


(define (repeat n fn r)
  (cond
   ((= n 1) (fn r))
   (#t (let ((tmp (fn r)))
         (fmt #t "level ~a completed~%" n)
         (repeat (- n 1) fn tmp)))))

  
   

;;(test "a" "b" "b") ;; ok

;;  ------ test suite does not like this TEST lisp form for some reason -- all give strings
;; --- BUG ? 
;; (test-group "right tests"
;;   (test "right ab" (right-half-str "ab") "a")
;;   (test "right abcd" (right-half-str "abcd") "ab")
;;   (test "right abcdef" (right-half-str "abcdef") "abc")
;;   (test "right abcdefgh" (right-half-str "abcdefgh") "abcd"))


(test-group "left tests"
  (test-assert "left ab" (equal? (left-half-str "ab") "a"))
  (test-assert "left abcd" (equal? (left-half-str "abcd") "ab"))
  (test-assert "left abcdef" (equal? (left-half-str "abcdef") "abc"))
  (test-assert "left abcdefgh" (equal? (left-half-str "abcdefgh") "abcd")))


(test-group "right tests"
  (test-assert "right ab" (equal? (right-half-str "ab") "b"))
  (test-assert "right abcd" (equal? (right-half-str "abcd") "cd"))
  (test-assert "right abcdef" (equal? (right-half-str "abcdef") "def"))
  (test-assert "right abcdefgh" (equal? (right-half-str "abcdefgh") "efgh")))


(test-group "blinks"
  (test-assert "eg1" (equal? (blink '(0 1 10 99 999))  '(1 2024 1 0 9 9 2021976)))

  (test-assert "a" (equal? (blink '(125 17)) '(253000 1 7))) 

  ;; After 1 blink:
  (test-assert "b" (equal? (blink '(253000 1 7)) '(253 0 2024 14168)))

  ;; After 2 blinks:
  (test-assert "c" (equal? (blink '(253 0 2024 14168)) '(512072 1 20 24 28676032)))

  ;; After 3 blinks:
  (test-assert "d" (equal? (blink '(512072 1 20 24 28676032))  '(512 72 2024 2 0 2 4 2867 6032)))

  ;; After 4 blinks:
  (test-assert "e" (equal? (blink '(512 72 2024 2 0 2 4 2867 6032))  
                           '(1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32)))

  ;; After 5 blinks:
  (test-assert "f" (equal? (blink '(1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32))  
                           '(2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2)))

  ;; After 6 blinks:
  (test-assert "g"(equal? (blink '(2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2))
                          '(20974 46912 28676032 40 48 4048 1 4048 8096 4 0 4 8 20 24 4 0 4 8 8 0 9 6 4048 16192 12144 14168 12144 1 6072 4048))))


(test-group "big blink tests"
  (test-assert "25 times on example" (equal? 55312 (length (repeat 25 blink '(125 17))))))

;; this should print the length of the solution 
(fmt #t "solution ~a ~%" (length (repeat 25 blink puzzle)))

;; the 2nd solution gets hopelessly lost on 75 blinks ... somewhere around 40 blinks ... combinatorial explosion
;; this should print the length of the solution 
(fmt #t "solution ~a ~%" (length (repeat 75 blink puzzle)))







  
