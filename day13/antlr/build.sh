#!/bin/bash

antlr4 Grammar.g4
javac *.java 

# run Grammar program 
# start with 's' as root of grammar 
# feed input.txt file into that configuration 
grun Grammar s -tree < ../input.txt > output.scm 







