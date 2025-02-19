

(*

read from a file a list of lines , lines contain two numbers

 *)

(* open TextIO; *)

(*read all lines from a file , collect them in order *)
(* val infile = openIn("../input.txt"); *)
(* inputLine(infile); *)
(* inputLine(infile); *)
(* inputLine(infile); *)

fun fac n =
    if n < 2 then 1 else n * fac (n - 1);

fac 5;

fac 10;

(*
functionally how read a value in ,

val lines = fn: string -> string list ;

*)




(* in is a reserved keyword  *)
fun lines f =
    lines2 (TextIO.openIn(f))
and lines2 ins = 	   
    let val v = TextIO.inputLine(ins) in
	case v of
	    NONE => []
	  | SOME c => c :: (lines2 ins)
    end;

    

			   
(* parsing is more work 			    *)
val mylines = lines "../input.txt";

(* string to two values *)
exception BadTwos ;

fun twos s =
    let val a = Int.fromString s in
	case a of
	    NONE => raise BadTwos
	  | SOME v1 => let val alen = String.size (Int.toString v1)
		           val blen = String.size s
		       in
			   let val b = Int.fromString (String.substring( s, alen, (blen - alen)))
			   in
			       case b of
				   NONE => raise BadTwos
				 | SOME v2 => (v1,v2)
			   end 
		       end
    end;
	
val input = List.map twos mylines;

(* here is value of having #1 mean 1st element of tuple *)
val lefts = List.map (#1) input;
val rights = List.map (#2) input;

(* listmergesort is a structure in smlnj extensions somewhere  *)
fun mysort xs = ListMergeSort.sort (fn (x,y) => (x > y)) xs;

(* distance apart is abs x - y *)
fun dist (x:int) (y:int) : int = Int.abs(x - y);

(* given two lists xs ys , compute dist absolute value of |xs.a - ys.ab| *)


fun pair f xs ys =
    if xs = [] then []
    else if ys = [] then []
    else let val (h::t) = xs			      
	     val (h2::t2) = ys in
	     (f h h2) :: (pair f t t2)
	 end;

val abss =
	let val l = mysort lefts
	    val r = mysort rights in
	    pair dist l r
	end;
    
val part1 = List.foldr (fn (x,y) => x + y) 0 abss;

fun occur f xs =
    occur2 f xs 0
and occur2 f xs n = 	   
    if xs = [] then n
    else let val (h::t) = xs in
	     if f(h) then (occur2 f t (n + 1))
	     else (occur2 f t n)
	 end;



(* number times 3 occurs in right list	      *)
val o3 = occur (fn x => x = 3) rights;	     

val part2a = List.map (fn x =>
			 let val ct = occur (fn y => x = y) rights
			 in (x,ct)
			 end)
		     lefts ;

val part2b = List.filter (fn x => not (#2(x) = 0)) part2a;

val part2c = List.map (fn x => (#1(x) * #2(x))) part2b;

val part2 = List.foldr (fn (x,y) => x + y) 0 part2c;






			    
	     

	    

			
			


		     

		  
	   

    


				 


		     

		    
		 
