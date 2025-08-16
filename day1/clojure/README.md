
# day 1 clojure

functional , but what if something goes wrong in the parsing of the file , 
where did it go wrong , what line , what column , what was the error ?
can we ignore it then , what is the next mistake we find 
is the file totally accepted ?

no tail call optimisation , or really just proper tail calls in the language
so no proper CPS conversion possible or runs out of stack space

that being said holding onto old closures may also cause same problem

coding in CPS is a little annoying from an algorithm standpoint , we specify every little
nuance and direction of each operation , 
can we not just specify broadly what algorithm does and find an appropriate implementation
to do that , 
check by execution that it meets the spec ?
spec driven code compilation construction ?

