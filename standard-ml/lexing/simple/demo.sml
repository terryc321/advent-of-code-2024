
open CalcLex;


(*
original
 val lexer = CalcLex.makeLexer (inputc (open_in "input.txt"));
*)

val open_in = TextIO.openIn;
val inputc = 0; (* ??? *)


val lexer = CalcLex.makeLexer (inputc (open_in "input.txt"));






			   
			   
