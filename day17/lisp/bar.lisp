
#|

now part 2 

|#


(defpackage :aoc
  (:use :cl))
(in-package :aoc)

;; out index , if outi does not match program then abort somehow ...
(defun run (a b c program)
  (let* ((program-length (length program))
	 (ip 0)
	 (inst nil)
	 (rand nil)
	 (combo nil)
	 (outi 0))
    (tagbody
     start
       (when (< ip 0) (go tape-end))
       (when (>= ip program-length) (go tape-end))
       ;; get instruction and operand 
       (setq inst (aref program ip))
       (setq rand (aref program (+ ip 1)))

       ;; recognised instruction 0
       (when (= inst 0) (go comp-adv))
       (when (= inst 1) (go comp-bxl))
       (when (= inst 2) (go comp-bst))
       (when (= inst 3) (go comp-jnz))       
       (when (= inst 4) (go comp-bxc))
       (when (= inst 5) (go comp-out))       
       (when (= inst 6) (go comp-bdv))       
       (when (= inst 7) (go comp-cdv))

       ;; fall through
       (go unrecognised-instruction)
       
       
     comp-adv
       ;; (format t "comp adv instruction ~a~%" inst)
       ;; value in combo register ?
       (cond
	 ((= rand 0) (setq combo 0))
	 ((= rand 1) (setq combo 1))
	 ((= rand 2) (setq combo 2))
	 ((= rand 3) (setq combo 3))
	 ((= rand 4) (setq combo a))
	 ((= rand 5) (setq combo b))
	 ((= rand 6) (setq combo c))
	 ;; 7 reserved
	 (t (go unrecognised-combo)))
       ;; a register
       (setq a (floor (/ a (expt 2 combo))))
       ;; next instruction pair        
       (setq ip (+ ip 2))
       (go start)
       

     comp-bxl
       ;; (format t "comp bxl instruction ~a~%" inst)
       (setq b (logxor b rand))
       ;; next instruction pair 
       (setq ip (+ ip 2))
       (go start)

     comp-bst
       ;; (format t "comp bst instruction ~a~%" inst)
       ;; value in combo register ?
       (cond
	 ((= rand 0) (setq combo 0))
	 ((= rand 1) (setq combo 1))
	 ((= rand 2) (setq combo 2))
	 ((= rand 3) (setq combo 3))
	 ((= rand 4) (setq combo a))
	 ((= rand 5) (setq combo b))
	 ((= rand 6) (setq combo c))
	 ;; 7 reserved
	 (t (go unrecognised-combo)))
       (setq b (mod combo 8))
       ;; next instruction pair 
       (setq ip (+ ip 2))
       (go start)

       
     comp-jnz
       ;; (format t "comp jnz instruction ~a~%" inst)
       ;; value in combo register ?
       (cond
	 ((= a 0)
	  ;; next instruction pair 
	  (setq ip (+ ip 2))
	  (go start))
	 (t
	  ;; set instruction pointer 
	  (setq ip rand)
	  (go start)))


      
     comp-bxc
       ;; (format t "comp bxc instruction ~a~%" inst)
       (setq b (logxor b c))		
       ;; next instruction pair 
       (setq ip (+ ip 2))
       (go start)


     comp-out
       ;; (format t "comp out instruction ~a~%" inst)
       ;; value in combo register ?
       (cond
	 ((= rand 0) (setq combo 0))
	 ((= rand 1) (setq combo 1))
	 ((= rand 2) (setq combo 2))
	 ((= rand 3) (setq combo 3))
	 ((= rand 4) (setq combo a))
	 ((= rand 5) (setq combo b))
	 ((= rand 6) (setq combo c))
	 ;; 7 reserved
	 (t (go unrecognised-combo)))

       ;; too many values
       (when (>= outi program-length)
	 (throw 'differ t))
       
       ;; check output matches ... may write too many values ...
       (let ((out (mod combo 8))
	     (val (aref program outi)))
	 (cond
	   ((/= out val)  (throw 'differ t))
	   (t (incf outi))))

       ;; next instruction pair 
       (setq ip (+ ip 2))
       (go start)
       
       
     comp-bdv
       ;; (format t "comp bdv instruction ~a~%" inst)
       ;; value in combo register ?
       (cond
	 ((= rand 0) (setq combo 0))
	 ((= rand 1) (setq combo 1))
	 ((= rand 2) (setq combo 2))
	 ((= rand 3) (setq combo 3))
	 ((= rand 4) (setq combo a))
	 ((= rand 5) (setq combo b))
	 ((= rand 6) (setq combo c))
	 ;; 7 reserved
	 (t (go unrecognised-combo)))
       
       ;; b register
       (setq b (floor (/ a (expt 2 combo))))
       ;; next instruction pair        
       (setq ip (+ ip 2))
       (go start)


     comp-cdv
       ;; (format t "comp cdv instruction ~a~%" inst)
       ;; value in combo register ?
       (cond
	 ((= rand 0) (setq combo 0))
	 ((= rand 1) (setq combo 1))
	 ((= rand 2) (setq combo 2))
	 ((= rand 3) (setq combo 3))
	 ((= rand 4) (setq combo a))
	 ((= rand 5) (setq combo b))
	 ((= rand 6) (setq combo c))
	 ;; 7 reserved
	 (t (go unrecognised-combo)))
       
       ;; c register
       (setq c (floor (/ a (expt 2 combo))))
       
       ;; next instruction pair        
       (setq ip (+ ip 2))
       (go start)

       
       
     unrecognised-instruction
       (format t "unrecognised instruction ~a~%" inst)
       (go end)

     unrecognised-combo
       (format t "unrecognised (rand) combo ~a~%" rand)
       (go end)

       
       
     tape-end
       (go end)
       
       
     end

       
       ;; halted
       )))


(defun example ()
  (let ((a (- 2024 1))
	(b 0)
	(c 0)
	(program #(0 3 5 4 3 0)))
    (catch 'done
      (loop while t do
	(catch 'differ
	  (incf a)
	  (run a b c program)
	  (throw 'done a))))))


(defun puzzle ()
  (let ((a 47792830)
	(b 0)
	(c 0)
	(program #(2 4 1 5 7 5 1 6 4 3 5 5 0 3 3 0)))
    (catch 'done
      (loop while t do
	(catch 'differ
	  (incf a)
	  ;; (format t "trying a = ~a ~%" a)
	  (run a b c program)
	  (throw 'done a))))))


;; parallelize this ?
;; eliminate 

    
















