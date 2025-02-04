


#|
Register A: 47792830
Register B: 0
Register C: 0

Program: 2,4,1,5,7,5,1,6,4,3,5,5,0,3,3,0

insutrction pointer ip starts at 0 and increments by 2 . if it reads outside range of input the computer halts

literal operands is just the number itself . literal operand 7 is 7 .
Combo operands 0 through 3 represent literal values 0 through 3.
Combo operand 4 represents the value of register A.
Combo operand 5 represents the value of register B.
Combo operand 6 represents the value of register C.

The adv instruction (opcode 0) performs division. The numerator is the
value in the A register. The denominator is found by raising 2 to the
power of the instruction's combo operand. (So, an operand of 2 would
divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The
result of the division operation is truncated to an integer and then
written to the A register.

The bxl instruction (opcode 1) calculates the bitwise XOR of register
B and the instruction's literal operand, then stores the result in
register B.

The bst instruction (opcode 2) calculates the value of its combo
operand modulo 8 (thereby keeping only its lowest 3 bits), then writes
that value to the B register.

The jnz instruction (opcode 3) does nothing if the A register is
0. However, if the A register is not zero, it jumps by setting the
instruction pointer to the value of its literal operand; if this
instruction jumps, the instruction pointer is not increased by 2 after
this instruction.

The bxc instruction (opcode 4) calculates the bitwise XOR of register
B and register C, then stores the result in register B. (For legacy
reasons, this instruction reads an operand but ignores it.)

The out instruction (opcode 5) calculates the value of its combo
operand modulo 8, then outputs that value. (If a program outputs
multiple values, they are separated by commas.)

The bdv instruction (opcode 6) works exactly like the adv instruction
except that the result is stored in the B register. (The numerator is
still read from the A register.)

The cdv instruction (opcode 7) works exactly like the adv instruction
except that the result is stored in the C register. (The numerator is
still read from the A register.)

|#

(defpackage :aoc
  (:use :cl))
(in-package :aoc)


(defun run (a b c program)
  (let* ((program-length (length program))
	 (ip 0)
	 (inst nil)
	 (rand nil)
	 (combo nil))
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
       
       (format t "~a," (mod combo 8))

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
  (let ((a 729)
	(b 0)
	(c 0)
	(program #(0 1 5 4 3 0)))
    (run a b c program)))

(defun puzzle ()
  (let ((a 47792830)
	(b 0)
	(c 0)
	(program #(2 4 1 5 7 5 1 6 4 3 5 5 0 3 3 0)))
    (run a b c program)))

;; (puzzle) => 2,1,3,0,5,2,3,7,1




;; The adv instruction (opcode 0) performs division. The numerator is the
;; value in the A register. The denominator is found by raising 2 to the
;; power of the instruction's combo operand. (So, an operand of 2 would
;; divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) The
;; result of the division operation is truncated to an integer and then
;; written to the A register.



  



    
















