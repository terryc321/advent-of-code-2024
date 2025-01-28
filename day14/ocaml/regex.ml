(* [@@@ocaml.warnings "-26-31-32"] *)

(*
(* .ocamlinit *)
#print_level 999999;;
#print_depth 99999;;
*)


(*
regular expression using Str module 

#require "graphics";;
#require "str";;

#require "bs" ;; no such package exists    
*)

#require "str";;

let r = Str.regexp {|hello \([A-Za-z]+\)|} in Str.replace_first r {|\1|} "hello world"

(*

let r = Str.regexp {|hello \([A-Za-z]+\)|} ;
   
val search_forward : regexp -> string -> int -> int
search_forward r s start searches the string s for a substring matching the regular expression r. The search starts at position start and proceeds towards the end of the string. Return the position of the first character of the matched substring.

Raises Not_found if no substring matches.

 val search_forward : regexp -> string -> int -> int
search_forward r s start searches the string s for a substring matching the regular expression r. The search starts at position start and proceeds towards the end of the string. Return the position of the first character of the matched substring.

Raises Not_found if no substring matches.
   
*)

(*
let r2 = Str.regexp {|\([+-]?[0-9]+\)|}   
let str = "aaaa123aaa"
let i1 =  Str.search_forward r2 str 0 
let m1 = Str.matched_string str
(* let i2 = Str.search_forward r2 str (String.length str) + i1 *)
(* let m2 = Str.matched_string str *)

let r2 = Str.regexp {|\([+-]?[0-9]+\)|}   
let str = "aaaa-+-123aaa"
let i1 =  Str.search_forward r2 str 0 
let m1 = Str.matched_string str
(* let i2 = Str.search_forward r2 str (String.length str) + i1 *)
(* let m2 = Str.matched_string str *)

(*
    lets see how to open a file and read a line
    https://ocaml.org/docs/file-manipulation

   module Stdlib
   int_of_string
   
  *)
  *)
exception No_values;;

let four_vals (str:string) : (int * int * int * int ) option  =
  try
    let reg = Str.regexp {|\([+-]?[0-9]+\)|} in
    let i1 =  Str.search_forward reg str 0 in
    let m1 = Str.matched_string str in
    (* print_string ("found " ^ m1 ^ " matched \n") ; *)
    let i2 = Str.search_forward reg str ((String.length m1) + i1)  in
    let m2 = Str.matched_string str in
    (* print_string ("found " ^ m2 ^ " matched \n"); *)
    let i3 = Str.search_forward reg str ((String.length m2) + i2)  in
    let m3 = Str.matched_string str in
    (* print_string ("found " ^ m3 ^ " matched \n"); *)
    let i4 = Str.search_forward reg str ((String.length m3) + i3)  in
    let m4 = Str.matched_string str in
    (* print_string ("found " ^ m4 ^ " matched \n"); *)
    (* flush stdout ; *)
    let x = int_of_string m1 in 
    let y = int_of_string m2 in
    let vx = int_of_string m3 in
    let vy = int_of_string m4 in
    Some (x,y,vx,vy) 
  with e ->
     None


let read_loop ic =
  let rec loop robots = (
    try 
      let line = input_line ic in
      (* read line, discard \n *)
      print_endline line;
      (* write the result to stdout *)
      flush stdout;
      (* write on the underlying device now *)
      let robot = four_vals line in
      match robot with
      | None -> raise No_values
      | Some z -> loop (z :: robots)
    with e -> match e with
             | End_of_file -> robots
             | z -> raise z
  ) in loop []



let spew robots = 
let oc = open_out "output.txt" in
  try
    let rec iter xs = (
      match xs with
      | [] -> ()
      | ((x,y,vx,vy) :: ys) ->
        (* print_string "hello!"; *)
         output_string oc ("p=" ^ string_of_int(x) ^ "," ^ string_of_int(y) ^ " v=" ^ string_of_int(vx) ^ "," ^ string_of_int(vy) ^"\n");
         iter ys ) in  iter robots ;
      flush stdout;
    (* close the output channel *)
    close_out oc 
    
  with e ->
    
    (* some unexpected exception occurs *)
    close_out_noerr oc;
    (* emergency closing *)
    raise e


    
(* this just runs let () = means ignore return values if any *)
let robots = 
  (* Read file and display the first line *)
  let ic = open_in "../input.txt" in
  try
    let robots = List.rev (read_loop ic) in
    close_in ic ;
    (* close the input channel *)

    (* output robots found to output.txt - consistency checked visually *)
    spew robots;

    (* get result to repl *)
    robots

  with e ->
    
    (* some unexpected exception occurs *)
    close_in_noerr ic;
    (* emergency closing *)
    raise e
