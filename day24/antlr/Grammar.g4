
// antlr-mode emacs
// %%%%%%%%%%%%%%%%%%%%%%%%%%%
// x00: 1
// x01: 0
// x02: 1
// x03: 1
// x04: 0
// y00: 1
// y01: 1
// y02: 1
// y03: 1
// y04: 1
// %%%%%%%%%%%%%%%%%%%%%%%%%%
// ntg XOR fgs -> mjb
// y02 OR x01 -> tnw
// kwq OR kpj -> z05
//
//  AND
//  XOR
//  OR
//
// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// read input file in directory above where we run the antlr4 command
// parses it , dumps resulting parse tree to output file , again one directory above
// antlr4 Grammar.g4 && javac *.java && grun Grammar s -tree < ../input > ../output

// in one big schebang ...
// > antlr4 Grammar.g4 && javac *.java && grun Holiday s -gui

//   notice on grun 's' is the root of the 
// > grun Holiday s -gui
// alpha beta 123
// charlie delta 345
// CTRL-d

// title of grammer MUST match the file name 
// Define a grammer called Grammar

grammar Grammar;		

s : r ( r )*  ;   
r : assigns | xor | or | and ;
WS : [ \t\r\n]+ -> skip ; 	
SYM : [a-z0-9]+ ;
bool : '0' | '1' ;
assigns : SYM   ':'   bool ;
xor : SYM 'XOR' SYM '->' SYM ;
and : SYM 'AND' SYM '->' SYM ;
or : SYM 'OR' SYM '->' SYM ;

