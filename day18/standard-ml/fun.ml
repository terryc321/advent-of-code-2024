
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

val pts = [(5,4),(4,2),(4,5),(3,0),(2,1),(6,3),(2,4),(1,5),(0,6),(3,3),(2,6),(5,1),(1,2),(5,5),(2,5),(6,5),(1,4),(0,4),(6,4),(1,1),(6,1),(1,0),(0,5),(1,6),(2,0)];

(*
List.hd
List.tl

6 x 6 grid all entries 0 initially
*)  
val ex = Array2.array(6,6,0);

fun up z = case z of
	     | z = (x,y) =>  (Array2.update(ex,x,y,1) ; 1)
	     | _ => 1				     
end



val _ = List.map(up , List.take(pts , 12));


		

	    
	    

	    
   
	    

    

	    
	    
