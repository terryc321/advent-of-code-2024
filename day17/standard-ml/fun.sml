
(*
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
*)

val ip : int ref = ref 0;
val regA : LargeInt.int ref = ref 729;
val regB : LargeInt.int ref = ref 0;
val regC : LargeInt.int ref = ref 0;
val prog : int list = [0,1,5,4,3,0]
val progLen = List.length prog 

val outp : InfInt list = [] ;

fun show () = List.rev outp ;

fun reset () = (ip := 0 ;
		regA := ref 0;
		() )


			  
(*
standard ml functional language makes clear distrinction between recursive and
non recursive procedures for some reason
could put everything a function fun takes void () and yields result 
*)
exception Combo
fun combo x =
    case x of
	0 => LargeInt.fromInt 0
     |  1 => LargeInt.fromInt 1
     |  2 => LargeInt.fromInt 2
     |  3 => LargeInt.fromInt 3
     |  4 => !regA
     |  5 => !regB
     |  6 => !regC
     |  _ => raise Combo

exception Halt
fun read i : int option =
    if i < 0 then NONE 
    else if i >= progLen then NONE
    else SOME (List.nth (prog,i));					     
		   
fun op_adv () = (* opcode 0  *)
    let val n = !regA (* numerator  *)
	val r = read (!ip + 1)		    
	val c = case r of  (* large int *)
		    NONE => raise Halt
		  | SOME v => combo v
	val d = IntInf.pow(2,Int.fromLarge c) (* denominator *)
	val dv = IntInf.divMod(n,d)			      
    in
	(regA := #1(dv) ;
	ip := !ip + 2 ;
	())   (* just get division div from tuple div mod  *)
    end;

	
fun op_bxl () = (* opcode 1  *)
    let val b = !regB 
	val r = read ((!ip) + 1)  (* literal operand *)
	val c = case r of  (* r for read  *)
		    NONE => raise Halt
		  | SOME v => v
	val d = IntInf.xorb(b, IntInf.fromInt c)
    in
	(regB := d ;
	ip := !ip + 2 ;
	())
    end;

	
fun op_bst () = (* opcode 2  *)
    let val r = read ((!ip) + 1)  (* literal operand *)
	val c = case r of  (* r for read  *)
		    NONE => raise Halt
		  | SOME v => combo v
	val d = IntInf.divMod(c, IntInf.fromInt 8)
    in
	(regB := #2(d) ;
	ip := !ip + 2 ;
	())  (* take 2nd of tuple the modulo divMod  *)
    end;


fun op_jnz () = (* opcode 3  *)
    if !regA = 0 then ()
    else let val r = read ((!ip) + 1)  (* literal operand *)
	     val c = case r of  (* r for read  *)
			 NONE => raise Halt
		       | SOME v => v	     
	 in
	     (ip := c ; (* ip not incremented by 2 *)
	     ())  (* set ip to value of literal operand  *)
	 end;


fun op_bxc () = (* opcode 4  *)
    let val r = read ((!ip) + 1)  (* literal operand *)
	val c = case r of  (* r for read  *)
		    NONE => raise Halt
		  | SOME v => v
	val d = IntInf.xorb(!regB , !regC)  (* bitwise xor register B C  *)
    in
	(regB := d ;
	ip := !ip + 2 ;
	())  
    end;
	
fun op_out () = (* opcode 5  *)
    let val r = read ((!ip) + 1)  (* literal operand *)
	val c = case r of  (* r for read  *)
		    NONE => raise Halt
		  | SOME v => combo v
    in
	(output c ;  (* simulate a terminal output values ...  *)
	ip := !ip + 2 ;
	())  (* take 2nd of tuple the modulo divMod  *)
    end;


fun op_bdv () = (* opcode 6  *)
    let val n = !regA (* numerator  *)
	val r = read (!ip + 1)		    
	val c = case r of  (* large int *)
		    NONE => raise Halt
		  | SOME v => combo v
	val d = IntInf.pow(2,Int.fromLarge c) (* denominator *)
	val dv = IntInf.divMod(n,d)			      
    in
	(regB := #1(dv) ;
	ip := !ip + 2 ;
	())   (* just get division div from tuple div mod  *)
    end;

		 
fun op_cdv () = (* opcode 7  *)
    let val n = !regA (* numerator  *)
	val r = read (!ip + 1)		    
	val c = case r of  (* large int *)
		    NONE => raise Halt
		  | SOME v => combo v
	val d = IntInf.pow(2,Int.fromLarge c) (* denominator *)
	val dv = IntInf.divMod(n,d)			      
    in
	(regC := #1(dv) ;
	ip := !ip + 2 ;
	())   (* just get division div from tuple div mod  *)
    end;

		 
		 
		 
