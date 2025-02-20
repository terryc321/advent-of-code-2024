

fun lines f =
    lines2 (TextIO.openIn(f))
and lines2 ins = 	   
    let val v = TextIO.inputLine(ins) in
	case v of
	    NONE => []
	  | SOME c => (chomp c) :: (lines2 ins)
    end
and chomp s =
    implode(List.rev(List.tl(List.rev(explode s))))
  

exception MyLines	   
val ss = fn s => String.tokens Char.isSpace s 
val strs = lines "../input.txt"
val lstrs = List.map ss strs
val fs = fn s => case Int.fromString s of
		     SOME y => y
		   | _ => raise MyLines				

val nums = List.map (fn x => List.map fs x) lstrs 

(* values all increasing or all decreasing *)
exception IncResult of bool;


(*			   
val inc xs = (case xs of
		 [] => true
	       | (h : t) => inc2 h t )
				 handle IncResult x => x
and inc2 h t = case t of
		   [] => raise IncResult true
		 | (h2 : t2) => if h >= h2 then (inc2 h2 t2)
				else raise IncResult false

val inc3 xs =  ( case xs of
		    [] => true
		  | (h:t) => inc2 h t )
and inc2 h t = case t of
		   [] => true
		 | (h2 : t2) =>  if h >= h2 then (inc2 h2 t2)
				 else false

recursive ? not allowed as a fn ?
val inc3 = fn xs =>
	      case xs of
		  [] => true
		| (h:t) => inc3 t ;

fun inc3 (xs: 'a list) : bool = case xs of
				    [] => true
				  | (h:t) => inc3 t
;
*)


(*

does not like this formulation 			   
fun inc xs = if xs = [] then true
	     else let val (h:t) = xs in
		      inc2 h t
		  end
and inc2 h t = if t = [] then true
	       else let val (h2:t2) = t
		    in
			if h <= h2 then inc2 h2 t2
			else false
		    end
*)

(*
fun inc h t = case t of
		  [] => true
		| (h2 : t2) => if h =< h2 then
				   inc h2 t2
			       else false
				      
fun.ml:77.11-77.13 Error: unbound type constructor: t2
fun.ml:77.23-77.25 Error: unbound variable or constructor: =<
fun.ml:78.15-78.17 Error: unbound variable or constructor: t2
*)

fun inc ho = case ho of
		 [] => true
	       | (h :: t) => inc2 h t
and inc2 ho to = case to of
		     [] => true
		   | (ho2 :: to2) => if ho < ho2 then
					 inc2 ho2 to2
				     else false ;


fun dec ho = case ho of
		 [] => true
	       | (h :: t) => dec2 h t
and dec2 ho to = case to of
		     [] => true
		   | (ho2 :: to2) => if ho > ho2 then
					 dec2 ho2 to2
				     else false ;


fun diff ho = case ho of
		 [] => true
	       | (h :: t) => diff2 h t
and diff2 ho to = case to of
		     [] => true
		   | (ho2 :: to2) => let val v = abs (ho - ho2) in
					 if v >= 1 andalso v <= 3 then
					     diff2 ho2 to2
					 else false
				     end;

					 
(*
some way to integrate tests ?

diff [1,2,3];
val it = true : bool
- diff [1,4,3];
val it = true : bool
- diff [1,4,7];
val it = true : bool
- diff [1,4,8];
val it = false : bool
- diff [1,4,1];
val it = true : bool
- diff [1,4,0];
val it = false : bool
- 
*)

val accept = fn xs => ((inc xs orelse dec xs) andalso diff xs);

(*
accept [1,2,3,4];
val it = true : bool
- accept [1,10 ,20,30];
val it = false : bool
- accept [1,~10,~20];
val it = false : bool
- accept [4,3,2,1];
val it = true : bool
- accept [12,9,6,3,0];
val it = true : bool
- accept [0,3,6,9,12];
val it = true : bool
- 
*)

val part1 = List.length (List.filter accept nums);

(* not tremendously good efficiency as scans each list 3 times
but works
val it = 421

 *)



(*
 statute of limitations do not believe fn can be recursive in standard ml
 must use fun keyword
*)


(*
can allow one bad level in a sequence of values so if badinc2 returns 0 or 1 that
is acceptable

fun badinc ho = case ho of
		 [] => true
	       | (h :: t) => let val v = badinc2 h t 0 in v < 2 end 
and badinc2 ho to bad = case to of
			    [] => bad
		   | (ho2 :: to2) => if ho <= ho2 then
					 badinc2 ho2 to2 bad
				     else badinc2 ho2 to2 (bad + 1)


fun baddec ho = case ho of
		 [] => true
		 | (h :: t) => let val v = baddec2 h t 0 in v < 2 end
and baddec2 ho to bad = case to of
			    [] => bad
		   | (ho2 :: to2) => if ho >= ho2 then
					 baddec2 ho2 to2 bad
				     else baddec2 ho2 to2 (bad + 1);


fun baddiff ho = case ho of
		 [] => true
	       | (h :: t) => let val v2 = baddiff2 h t 0 in v2 < 2 end 
and baddiff2 ho to bad = case to of
			     [] => bad
		   | (ho2 :: to2) => let val v = abs (ho - ho2) in
					 if v >= 1 andalso v <= 3 then
					     baddiff2 ho2 to2 bad
					 else baddiff2 ho2 to2 (bad + 1)
				     end;



val accept2 = fn xs => let val a = badinc xs 
			   val b = baddec xs
			   val c = baddiff xs in
			   if not c then false
			   else if not a then b
			   else true				    
		       end
			   	    
val part2b = List.filter (fn x => not (accept2 x)) nums;
val part2a = List.filter accept2 nums;
val part2 = List.length part2a;
*)


(*
ok so review , solved wrong problem
really want to be able to remove one index from sequence and have  that new sequence
pass the tests
*)
fun removeItem xs n = if n < 1 then xs
		      else if xs = [] then xs
		      else if n = 1 then List.tl xs
		      else (List.hd xs) :: (removeItem (List.tl xs) (n - 1));


(* this is unfixable  *)
exception Fix of (int list) option;

fun fixable f xs = (fixable2 f xs 0 ((List.length xs) + 2))
and fixable2 f xs n len = if n < len then
			      let val r = removeItem xs n (* try remove nth item *)
				  val ok = f r (* possible *)
			      in if ok then SOME r (* (r,n + 1,List.nth(xs,n),xs) *)
				 else fixable2 f xs (n + 1) len
			      end				  
			  else NONE

fun safe xs = fixable accept xs;


val part2 =
    let val s = List.map safe nums
	val f = fn x => case x of
			    NONE => false
			  | _ => true
    in
	List.filter f s 
    end


val sum = List.foldr (fn (x,y) => x + y) 0

val fidelity = sum (List.map (fn x => sum x) nums)
		   

	    
		

		   

				     
      (*
val part2a = List.map (fn x => fixable accept x) nums
val part2 = List.length (List.filter (fn x => x) part2a)		      
      *)

(* val part2a = List.filter accept2 nums; *)
(* val part2 = List.length part2a; *)

(* still get 570 whats going on ?  *)








			

			
		      
						  

				





				      

		  
		  
		  

(*  
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
*)

  





			    
	     

	    

			
			


		     

		  
	   

    


				 


		     

		    
		 
