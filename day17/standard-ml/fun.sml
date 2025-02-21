


(*
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
*)


structure aoc = 
struct
	      
val ip : int ref = ref 0;
val regA : LargeInt.int ref = ref 729;
val regB : LargeInt.int ref = ref 0;
val regC : LargeInt.int ref = ref 0;
val prog : int list ref = ref [0,1,5,4,3,0]
val progLen = List.length (!prog)

			  
(* dont think outp is even used - but it sure was difficult to get past type checker*)
val outp : IntInf.int list ref = ref [] ; 

val halted : bool ref = ref false;

(*
standard ml functional language makes clear distrinction between recursive and
non recursive procedures for some reason
could put everything a function fun takes void () and yields result 
*)
exception Combo
exception BadOp 
exception Halt 

fun setProg p = (
    prog := p ;
    progLen = List.length (!prog);
    ()
)
and output c = (outp := (c :: (!outp)) ; ())
and result () = List.rev (!outp) 
and reset () = (
    halted := false;
    ip := 0 ;
    regA := LargeInt.fromInt 729;
    regB := LargeInt.fromInt 0;
    regC := LargeInt.fromInt 0;
    outp := [] ;
    () )
and combo x =
    case x of
	0 => LargeInt.fromInt 0
     |  1 => LargeInt.fromInt 1
     |  2 => LargeInt.fromInt 2
     |  3 => LargeInt.fromInt 3
     |  4 => !regA
     |  5 => !regB
     |  6 => !regC
     |  _ => raise Combo

and exec () =
    (())

	
and step () = (
      if !halted then (print "halted ")
      else
	  let val r = read (!ip)
	  in
	      case r of  
		  NONE => halted := true 
		| SOME v => op_step v
			    handle Halt => (halted := true ; ())
	  end ;
      ())
	
and status () = (
    print "status : ";
    print " Halted ? " ; print (Bool.toString (!halted));
    print " : ip = ";  print (Int.toString (!ip));
    print " : A=";  print (IntInf.toString (!regA));
    print " : B=";  print (IntInf.toString (!regB));
    print " : C=";  print (IntInf.toString (!regC));    
    print "\n";
    ()
    )

and op_step v =
    (
      (*
      print "step : v = ";  print (Int.toString v);
      print " : ip = ";  print (Int.toString (!ip));
      print "\n";
      *)
      
    case v of
	0 => op_adv () 
      | 1 => op_bxl () 
      | 2 => op_bst ()
      | 3 => op_jnz () 
      | 4 => op_bxc () 
      | 5 => op_out ()
      | 6 => op_bdv () 
      | 7 => op_cdv () 
      | _ => raise BadOp )

and read i : int option =
    if i < 0 then NONE 
    else if i >= progLen then NONE
    else SOME (List.nth (!prog,i))					     
and halt () =
    (halted := true ; ())
	
and op_adv () = (* opcode 0  *)
    (
      let val n = !regA (* numerator  *)
	  val r = read ((!ip) + 1)
	  (*val _ = (print "reading ip at " ; print (Int.toString (!ip)) ; print "\n")*)
	  val c = case r of  (* large int *)
		      NONE => (halt () ; raise Halt)
		    | SOME v => combo v
	  val d = IntInf.pow(2,Int.fromLarge c) (* denominator *)
	  val dv = IntInf.divMod(n,d)
	  val res = #1(dv)
      in
	  regA := res
      end ;
      (*
      print "executed adv : setting regA to ";
      print (IntInf.toString (!regA)) ;
      print "\n";
*)
      ip := (!ip + 2) ;
      (*
      print "ip advanced to  ";
      print (Int.toString (!ip)) ;
      print "\n";
*)
      ())   (* just get division div from tuple div mod  *)
    
    

and op_bxl () = (* opcode 1  *)
    let val b = !regB 
	val r = read ((!ip) + 1)  (* literal operand *)
	val c = case r of  (* r for read  *)
		    NONE => (halt () ; raise Halt)
		  | SOME v => v
	val d = IntInf.xorb(b, IntInf.fromInt c)
    in
	(regB := d ;
	 (*
	 print "executed bxl : setting regB to ";
	 print (IntInf.toString d) ;
	 print "\n";
*)
	 ip := !ip + 2 ;
(*	 print "ip advanced to  ";
	 print (Int.toString (!ip)) ;
	 print "\n";
*)
	 ())
    end
	
and op_bst () = (* opcode 2  *)
    let val r = read ((!ip) + 1)  (* literal operand *)
	val c = case r of  (* r for read  *)
		    NONE => (halt () ; raise Halt)
		  | SOME v => combo v
	val d = IntInf.divMod(c, IntInf.fromInt 8)
    in
	(regB := #2(d) ;
	ip := !ip + 2 ;
	())  (* take 2nd of tuple the modulo divMod  *)
    end
	
and op_jnz () = (* opcode 3  *)
    if (!regA) = (LargeInt.fromInt 0) then (ip := !ip + 2 ; ())
    else let val r = read ((!ip) + 1)  (* literal operand *)
	     val c = case r of  (* r for read  *)
			 NONE => (halt () ; raise Halt)
		       | SOME v => v	     
	 in
	     (ip := c ; (* ip not incremented by 2 *)
	     ())  (* set ip to value of literal operand  *)
	 end
	     
and op_bxc () = (* opcode 4  *)
    let val r = read ((!ip) + 1)  (* literal operand *)
	val c = case r of  (* r for read  *)
		    NONE => (halt () ; raise Halt)
		  | SOME v => v
	val d = IntInf.xorb(!regB , !regC)  (* bitwise xor register B C  *)
    in
	(regB := d ;
	ip := !ip + 2 ;
	())  
    end
and op_out () = (* opcode 5  *)
    (
      let val r = read ((!ip) + 1)  (* literal operand *)
	  val c = case r of  (* r for read  *)
		      NONE => (halt () ; raise Halt)
		    | SOME v => combo v	
	  val dv = IntInf.divMod(c,IntInf.fromInt 8)
	  val m = #2 dv (* take 2nd of tuple the modulo divMod  *)
	  val s = IntInf.toString m
      in
	  (print s ;  (* output combo operand  modulo 8 *)
	   print " , " ;
	  output m  (* record output in our makeshift list *))
      end ;      
      ip := ((!ip) + 2) ;      
      ())

and op_bdv () = (* opcode 6  *)
    let val n = !regA (* numerator  *)
	val r = read (!ip + 1)		    
	val c = case r of  (* large int *)
		    NONE => (halt () ; raise Halt)
		  | SOME v => combo v
	val d = IntInf.pow(2,Int.fromLarge c) (* denominator *)
	val dv = IntInf.divMod(n,d)			      
    in
	(regB := #1(dv) ;
	ip := !ip + 2 ;
	())   (* just get division div from tuple div mod  *)
    end
and op_cdv () = (* opcode 7  *)
    let val n = !regA (* numerator  *)
	val r = read (!ip + 1)		    
	val c = case r of  (* large int *)
		    NONE => (halt () ; raise Halt)
		  | SOME v => combo v
	val d = IntInf.pow(2,Int.fromLarge c) (* denominator *)
	val dv = IntInf.divMod(n,d)			      
    in
	(regC := #1(dv) ;
	ip := !ip + 2 ;
	())   (* just get division div from tuple div mod  *)
    end
and run () = (
    if (!halted) then raise Halt
    else (step (); run ())
    )
and test1 () = (
    (* If register C contains 9, the program 2,6 would set register B to 1. *)
    reset () ;
    regC := LargeInt.fromInt(9) ;
    setProg([2,6]); 
    step ();
    !regB
)
and test2 () = (
    (* If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2. *)
    reset () ;
    regA := LargeInt.fromInt(10) ;
    setProg([5,0,5,1,5,4]); 
    run () handle Halt => ()
)		   
and test3 () = (
    (* If register A contains 2024, the program 0,1,5,4,3,0 would output
       4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
     *)
    reset () ;
    regA := LargeInt.fromInt(2024) ;
    setProg([0,1,5,4,3,0]); 
    run () handle Halt => ();
    !regA
)
and test4 () = (
    (* If register B contains 29, the program 1,7 would set register B to 26.		   *)
    reset () ;
    regB := LargeInt.fromInt(29) ;
    setProg([1,7]); 
    run () handle Halt => ();
    !regB
)
and example () = (
    reset () ;
    regA := LargeInt.fromInt(729) ;
    setProg([0,1,5,4,3,0]); 
    run () handle Halt => ();
    result()
)
and part1 () = (
    reset () ;
    regA := LargeInt.fromInt(47792830) ;
    setProg([2,4,1,5,7,5,1,6,4,3,5,5,0,3,3,0]); 
    run () handle Halt => ();
    result()
    )
	
end;





    
		 
		 
