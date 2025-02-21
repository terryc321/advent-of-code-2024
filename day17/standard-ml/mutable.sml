
(* == mutable variables === *)
val ip : int ref = ref 0 ;

fun exec () =
    (ip := (!ip) + 2 ; () );

fun exec2 () =
   let val a = 2
   in
	(ip := (!ip) + a ; () )
   end;


		 
