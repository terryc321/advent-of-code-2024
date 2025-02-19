

val g = Array2.array(5,5,0);

(*
Array2.dimensions(g); 
Array2.sub(g,1,1);
Array2.update(g,1,1,3);
*)


(* some io fun 	 
fun read file =
let val inStream = TextIO.openIn file

in
    TextIO.input1 inStream ;
end
*)

(* standard ml 2d array is  Array2.sub(Y,X) and Y 0 to N-1 , Z 0 to N-1 *)
val k = [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]];

val h = Array2.fromList(k);

(* example is a 7 x 7 grid  all initially values 0 , width height ranges 0 to 6 inclusive range of 7*)
val ex = Array2.array(7,7,0);

val pts = [(5,4),(4,2),(4,5),(3,0),(2,1),(6,3),(2,4),(1,5),(0,6),(3,3),(2,6),(5,1),(1,2),(5,5),(2,5),(6,5),(1,4),(0,4),(6,4),(1,1),(6,1),(1,0),(0,5),(1,6),(2,0)];

val twelve = List.take(pts,12);


fun showgrid g =
    let val hgt = Array2.nCols g
	val wid = Array2.nRows g
	fun iter y x = if x > (wid -1) then (print "\n"; iter (y+1) 0)
		       else if y > (hgt - 1) then ()
		       else let val n = Array2.sub(g,y,x)
			    in if n = 0 then (print "." ; iter y (x+1))
			       else if n < 0 then (print "#" ; iter y (x+1))
			       else (print "O" ; iter y (x+1))
			    end
    in iter 0 0
    end;


(* makes a fresh copy of 2d grid *)
fun copygrid h =
    let val hgt = Array2.nCols(h)
	val wid = Array2.nRows(h)
    in 
	let val g = Array2.array(hgt,wid,0)
	in
	    let fun iter y x = if x > (wid -1) then iter (y+1) 0
			       else if y > (hgt - 1) then g
			       else let val n = Array2.sub(h,y,x)						      
				    in (Array2.update(g,y,x,n);
					iter y (x+1))
				    end								       
	    in iter 0 0
	    end
	end
    end;
	



       

fun show g title =  (print title;
		     print "\n";
		     let val wid = 6
			 val hgt = 6
			 val init_x = 0
			 val init_y = 0
		     in (showgrid g ;
			 print "\n";
			())
		     end);

			 


fun process g xs = (* use negative value ~1  standard ml weird squiggle for negative 1 *)
    let fun up g y x = Array2.update(g,y,x,~20) 
    in
	case xs of
	    [] => () 
	 |  ( (x,y) :: t ) => (up g y x  ; process g t ; ())
    end
		

val ex2 = Array2.array(7,7,0);

val _ = process ex2 (List.take( pts, 12));

(*
search ex2 to find a route from top left (0 ,0) to bottom right (6,6) inclusive
simple breadth first search ??
off board
can i go here ? may be a wall # 

 y x travelled dist d , if d < this square has value 
 *)
val ex3 = Array2.array(7,7,0);


fun search c =
    let val g = copygrid c
	val best = ref 0 		    
    in
	let val hgt = Array2.nCols( g)
	    val wid = Array2.nRows( g)
	    fun iter( y,x ,d) =        (* after found a solution set best = d  *)
		if (y = 6 andalso x = 6) then (show g "solution ";
					       print ("solution d=" ^ ((Int.toString d)));
					       print (" : best =" ^ ((Int.toString (!best)) ^ "\n"));
					       if ((!best) = 0) then (best := d ; () )
					       else if d < !best then (best := d ; ())
									  else ())
		else if ((!best) > 0 andalso (d > (!best))) then () (* too far  *)
	        else if x > (wid -1) then ()
		else if x < 0 then ()
		else if y > (hgt - 1) then ()
		else if y < 0 then ()
		else let val n = Array2.sub(g ,y,x)
		in
		    if n < 0 then () (*cannot go here*)
		    else if n > 0 then () (* already gone here *)
		    else (Array2.update(g,y,x,d) ;  (* assert gone here *)
			  iter( (y-1), x, (d+1)) ;
			  iter( (y+1), x, (d+1)) ;
			  iter( y ,(x-1), (d+1)) ;
			  iter( y ,(x+1), (d+1)) ;
			  Array2.update(g,y,x,0)  (* retract here *)
			 )
		end				      
	in let val x = 0
	       val y = 0
	       val d = 1 (*say first move is at 0,0*)
	   in
	       iter( y, x, d) ;
	       !best - 1 
	   end	   
	end
    end;




	


				 


		     

		    
		 
