//  rather than using regular expressions we can just use antlr software 
//  to properly parse the input file
//
//
// Button A: X+29, Y+71
// Button B: X+52, Y+31
// Prize: X=5388, Y=4716
// 
// Button A: X+47, Y+16
// Button B: X+14, Y+21
// Prize: X=4795, Y=2996
// 
// Button A: X+21, Y+11
// Button B: X+12, Y+60
// Prize: X=741, Y=2859
//
//

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
r : buttonAs buttonBs prizeCs ;
buttonAs : buttonA colon xplus INT comma yplus INT ;
buttonBs : 'Button B: X+' INT comma yplus INT ;
prizeCs  : 'Prize: X=' INT ', Y=' INT ;
WS : [ \t\r\n]+ -> skip ; 	
INT : [0-9]+ ; 	

buttonA : 'Button A' ;
xplus : 'X+' ;
yplus : 'Y+' ;
colon : ':' ;
semi : ';' ;
comma : ',' ; 




// ID : [A-Za-z]+ ;			// match lower case identifiers

// to : 'to' ;
// eq : '='  ;
// button : 'Button' ;
// prize : 'Prize' ;
// xeq : 'X=' ;
// yeq : 'Y=' ;
// comma : ',' ;
// semi : ';' ;

// ignore whitespace 
// WS : [ \t\r\n]+ -> skip ; 	

// integers 
// INT : [0-9]+ ; 	


