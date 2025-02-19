
datatype lexresult=  BREAK |  NUM of int | EOF

val std_out = TextIO.stdOut
val output = TextIO.output

(* found this definition https://wiki.c2.com/?SmlLanguage *)
fun revfold _ nil b = b 
 | revfold f (hd::tl) b = revfold f tl (f(hd,b));
		  
val linenum = ref 1
val error = fn x => output(std_out,x ^ "\n")
 val eof = fn () => EOF
     
%%
 
%structure CalcLex

digit=[0-9];

ws = [\ \t];

%%

\n       => (BREAK); 

{ws}+    => (lex());

{digit}+ => (NUM (revfold (fn(a,r)=>ord(a)-ord(#"0")+10*r) (explode yytext) 0));

.        => (error ("calc: ignoring bad character "^yytext); lex());



