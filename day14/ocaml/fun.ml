
(*
ppart 2 is about see-ing a christmas tree , possibly some measure of how disconnected the image is 
perhaps we should somehow track if pixels are close together ? less noise ?? 
*)

(* importing Unix module after open Core , allows us to find the Unix.sleepf   *)

(* more documentation should be avaialble on how to use ocaml to get best out of it  *)

(* more docs on espeicalliy the nasty match error on lists is terrible *)

(* how read all lines from file in ocaml *)

(* 
#require "core";;
open Core;;

#require "graphics";;
open Graphics;;

#use "fun.ml";;


--- ignore following
--- open Core.Std gives an error 
----#use "core";; lowercase ...??
*)

#require "core";;
open Core ;;

#require "Unix";;
module Unix = UnixLabels;;

(* make graphics available by 
> opam install graphics
*)
#require "graphics";;

let read file = In_channel.read_lines file ;;

(* 
a trivially ball busting parser for p=69,95 v=70,-27 
 drop p= int comma int space v= int comma int 
 int could be - N or just N 
 no + N positives 
 ints are small ints 

 function takes a string returns four numbers position and velocity 
                                        (x,y,vx,vy) 
*)

(* val twice : float -> float  *)
let twice_f (x : float) : float = x +. x ;;
let twice_i (x : int) : int = x + x ;;

(* trying to break down string into an int , if starts with - then negative result 
recursive , also 

mutually recursive routines which is painful , cannot just write distinct procedures 
assume handle simplest case single character of a string

let parse_int (s : string) : int = 
  if String.get s 0 = '-' then parse_int2 (String.sub s ~pos: 1 ~len:(String.length s)) false
  else parse_int2 s true 
and parse_int2 (s : string) (b : bool) (sum : int) : int = 
  if s = "" then 
  if String.get s 0 = '-' then parse_int2 (String.sub s ~pos: 1 ~len:(String.length s)) false

keep recomputing string length over and over again ...
*)

let ps (s:string) (sum:int) : int = sum ;;



exception EmptyString;;
exception BadCharacter;;

(* does not work for larger ints  
 computes string length a couple of times then decrements thereafter 
 doing lot of string substring perhaps better to just use an index i into String 
 maybe i made a mistake there ..

so we can get an int from a string that should be an int 
now we need to  
how do we profile the code , 
how can we tell its doing large amounts of copying ? or taking too much time ?  

*)
let rec parse_int (s :string) : int = 
 if String.is_empty s then raise EmptyString 
 else let rs = (String.sub s ~pos:1 ~len: (String.length s - 1)) in
       let ch = (String.get s 0) in  match ch with        
       | '-' -> let result = parse_int0 rs in (- result)
       | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> parse_int0 s 
       | _ -> raise BadCharacter 
and parse_int0 (s : string) : int = 
 if String.is_empty s then raise EmptyString 
 else let slen_1 = ((String.length s) - 1) in
      let rs = (String.sub s ~pos:1 ~len: slen_1) in
      let ch = (String.get s 0) in  match ch with        
       | '0' -> parse_int2 rs 0 slen_1
       | '1' -> parse_int2 rs 1 slen_1
       | '2' -> parse_int2 rs 2 slen_1
       | '3' -> parse_int2 rs 3 slen_1
       | '4' -> parse_int2 rs 4 slen_1
       | '5' -> parse_int2 rs 5 slen_1
       | '6' -> parse_int2 rs 6 slen_1
       | '7' -> parse_int2 rs 7 slen_1
       | '8' -> parse_int2 rs 8 slen_1
       | '9' -> parse_int2 rs 9 slen_1
       | _ -> raise BadCharacter 
and  parse_int2 (s : string) (sum : int) (slen : int) : int = 
 if String.is_empty s then sum
 else let slen2 = slen - 1 in
      let rs = (String.sub s ~pos:1 ~len: slen2) in
      let ch = (String.get s 0) in  match ch with        
       | '0' -> parse_int2 rs (0 + (sum * 10)) slen2
       | '1' -> parse_int2 rs (1 + (sum * 10)) slen2
       | '2' -> parse_int2 rs (2 + (sum * 10)) slen2
       | '3' -> parse_int2 rs (3 + (sum * 10)) slen2
       | '4' -> parse_int2 rs (4 + (sum * 10)) slen2
       | '5' -> parse_int2 rs (5 + (sum * 10)) slen2
       | '6' -> parse_int2 rs (6 + (sum * 10)) slen2
       | '7' -> parse_int2 rs (7 + (sum * 10)) slen2
       | '8' -> parse_int2 rs (8 + (sum * 10)) slen2
       | '9' -> parse_int2 rs (9 + (sum * 10)) slen2
       | _ -> raise BadCharacter ;;


(* example line to parse 

p=69,95 v=70,-27
p=95,51 v=-76,-2

probably have outer function define string s , lim string length limit , i index into string s 
make it look a bit neater 

just collects small ints 
if integers are too large for small int then the error will go undetected !? !? 

*)
  
let rec sp (s:string) = 
    sp2 s 0 (String.length s) [] 
and  sp2 (s:string) (i :int)  (lim : int) (vals : int list) : int list = 
  if i = lim then vals
  else let i2 = i + 1 in 
       let neg = false in 
       let ch = (String.get s i) in  match ch with        
       | '0' -> sp3 s i2 lim vals 0 neg
       | '1' -> sp3 s i2 lim vals 1 neg
       | '2' -> sp3 s i2 lim vals 2 neg
       | '3' -> sp3 s i2 lim vals 3 neg
       | '4' -> sp3 s i2 lim vals 4 neg
       | '5' -> sp3 s i2 lim vals 5 neg
       | '6' -> sp3 s i2 lim vals 6 neg
       | '7' -> sp3 s i2 lim vals 7 neg
       | '8' -> sp3 s i2 lim vals 8 neg
       | '9' -> sp3 s i2 lim vals 9 neg
       | '-' -> sp3 s i2 lim vals 0 true
       (* | '-' -> sp3 s i lim vals 0 *)
       | _ -> sp2 s (i + 1) lim vals 
and  sp3 (s:string) (i :int)  (lim : int) (vals : int list) (sum : int) (neg : bool) : int list = 
  if i = lim then if neg then vals @ [-sum] 
                  else vals @ [sum] 
  else let i2 = i + 1 in 
       let ch = (String.get s i) in  match ch with        
       | '0' -> sp3 s i2 lim vals (0 + (sum * 10)) neg
       | '1' -> sp3 s i2 lim vals (1 + (sum * 10)) neg
       | '2' -> sp3 s i2 lim vals (2 + (sum * 10)) neg
       | '3' -> sp3 s i2 lim vals (3 + (sum * 10)) neg
       | '4' -> sp3 s i2 lim vals (4 + (sum * 10)) neg
       | '5' -> sp3 s i2 lim vals (5 + (sum * 10)) neg
       | '6' -> sp3 s i2 lim vals (6 + (sum * 10)) neg
       | '7' -> sp3 s i2 lim vals (7 + (sum * 10)) neg
       | '8' -> sp3 s i2 lim vals (8 + (sum * 10)) neg
       | '9' -> sp3 s i2 lim vals (9 + (sum * 10)) neg
       (* | '-' -> sp3 s i lim vals 0 *) 
       | _ -> if neg then sp2 s i lim (vals @ [-sum]) 
                     else sp2 s i lim (vals @ [sum]) 

;;
 (* #quit ;; 
   ~f:sp is way to tell ocaml that sp is the function from jane street core library 
*)
 
(* let parse (s : string) : (int * int * int * int) = (1,2,3,4);; *)
(* List.map ~f:parse (read "../input.txt") ;; *)
(* get a substring 
    String.sub "asdf" ~pos:2 ~len:2 ;; *)

(* a record type is a very poor adt 
type robot = {
  x : int;
  y : int;
  vx : int;
  vy : int;
}
*)
type robot =
  | Robot of int * int * int * int (* x, y, vx, vy *)

(* let r : robot = { x = 0; y = 0; vx = 1; vy = 1 };; *)

exception BadRobot;;

(*
let to_robot (xs : int list) : robot = 
  if List.length xs = 4 then match xs with 
                     | a :: (b :: (c :: (d :: []))) ->
                          let (r : robot) = { x= a ; y = b ; vx = c ; vy = d } in r 
                     | _ -> raise BadRobot 
  else raise BadRobot ;;
*)
let to_robot (xs : int list) : robot = 
  if List.length xs = 4 then match xs with 
                     | a :: (b :: (c :: (d :: []))) -> Robot(a,b,c,d)                          
                     | _ -> raise BadRobot 
  else raise BadRobot ;;



let vals = List.map ~f:to_robot (List.map (read "../input.txt") ~f:sp);;

let ex_vals = List.map ~f:to_robot (List.map (read "../example.txt") ~f:sp);;

(*
(* .ocamlinit *)
#print_level 999999;;
#print_depth 99999;;
*)


(* what does it mean for the robot to teleport ?  *)

(* 
ended up with a list of robots , well a list of robot options Some 
but if pattern match succeeded then the routine 
*)


(* show a grid 11 tiles wide , 7 tile tall  *)

(*
given some robots  p=2,4 v=2,-3  [2;4;2;-3] after each second show where the robot is on the grid 
including teleporting

print_endline "" 
print_char '\n'  

show_grid return void so just side effects 
*)


(*
cannot pattern match against a list datastructure for some reason 
let has_robot (x:int) (y:int) (robots: int list list) : bool = 
  if List.is_empty robots then false 
  else let others = List.tl robots in
       let head = List.hd robots in 
       match head with 
       | rx :: (ry :: (vx :: (vz :: _))) -> if rs = x && ry = y then true
                          else has_robot x y others 
       | _ -> BadRobot ;;

cannot do a  pattern match like this 
(_ :: a) = [] ;;

how do i turn a list of 4 numbers into Robots ? 
[ 1;2;3;4] -> Robot x:1 y:2 vx:3 vy:4 
*)


(*
let rec has_robot sx sy robots : bool = 
  if List.is_empty robots then false 
  else let others = List.tl robots in
       let r = List.hd robots in
       match r with        
       | { x ; y ; vx ; vy } -> if x = sx && y = sy then true else has_robot sx sy others 
       | _ -> has_robot sx sy others ;;
*)
(*
let rec has_robot (sx : int) (sy : int) robots : bool = 
  if List.is_empty robots then false 
  else match List.hd robots with 
       | Some { x ; y ; vx ; vy } -> true 
       | None -> let others = List.tl robots in 
                  match others with 
                 | Some _ -> has_robot sx sy others 
                 | None -> false ;;
*)

(*   
       let r : robot = List.hd robots in
       if r = Some { x ; y ; vx ; vy } then 
            if r.x = sx && r.y = sy then true 
            else has_robot sx sy others 
       else has_robot sx sy others ;;
*)


(*
let rec has_robot (sx : int) (sy : int) robots : bool = 
  if (phys_equal robots []) then false 
  else match List.hd robots with 
       | Some { x ; y ; vx ; vy } -> true 
       | _ -> has_robot sx sy (List.tl robots) ;;

*)





(*
keep coming up against problem of 4 items in a list , i am ok , otherwise not . 
filter out those that are not , just left with a list of i am ok';s .
just want a list of robots , but pattern match on list is not exhaustive or leaving me in 
a situtation where have a list of robot option 
  Some robot
  None 
how pattern match against some robot ?
*)






(*
ocaml handles options in a weird way 
 say { x = 0 ; y = 0 ; vx = 1 ; vy = 2 } ;; 
 no mention this is a robot anywhere but type inference says its so ..??
- : robot = {x = 0; y = 0; vx = 1; vy = 2}

*)




(* filter any robot that is on row 1 say , then when printing that row , look only at those ...
rather than scanning all robots over again ... possibly 


let has_robot (x:int) (y:int) (robots: robot list) : bool = 
  if List.is_empty robots then false 
  else let others = List.tl robots in
       let head = List.hd robots in 
       let rx :: _  = head in 
       let _ :: ry :: _ = head in
       if rs = Some x && ry = y then true
                          else has_robot x y others 
       | _ -> BadRobot ;;



let show_grid (x:int) (y:int) (wid:int) (hgt:int) (robots: int list list) : () = 
  letrec help (x :int) (y:int) : () = 
      if (x > wid) then help 0 (y + 1) wid hgt 
      else if (y > hgt) then ()
      else if x = 0 then let () =  print_endline "" in
                         let () = 
 
*)

(*
let rec has_robot (x:int) (y:int) robots : bool = 
  match robots with 
  | (Some (Robot (a,b,c,d))) :: rest -> if x = a && y = b then true else has_robot x y (List.tl robots)
  | [] -> false ;;
*)

(* omg only taken me about 10 hours to write a pattern match 
the secret is on the pattern match , eg hd :: tl use tl rather than List.tl robots 
  List.tl robots is an option type 
  whereas rest in A :: rest is just a' List 
*)

(* WOW eventually this below worked using the List parts that matched , and not using List 

let rec has_robot (x:int) (y:int) robots : bool = 
  match robots with 
  | (Some (Robot (a,b,c,d))) :: rest -> if x = a && y = b then true else has_robot x y rest
  | None :: rest -> has_robot x y rest 
  | [] -> false ;;
*)

(* filter robots , all robots that are on row 1 for example  *)
(* filter robots , all robots that are at x y  *)

(*
do not make mistake that if pattern match Robot(a,b,c,d) creates new bindings for a b c d 
    so if we use Robot(x,y,vx,vy) and x is outside scope , shadows x and so cannot use 
   original x 
*)
let has_robot (x: int) (y:int) seq =
List.filter seq ~f:(fun r -> match r with 
                            |  Robot(a,b,_,_) -> if x = a &&  y = b then true else false) ;;


let rec show_grid (wid:int) (hgt:int) (robots : robot list) = 
     show_grid2 0 0 wid hgt robots 
and
 show_grid2 (x:int) (y:int) (wid:int) (hgt:int) (robots : robot list) = 
  if (x >= wid) then show_grid2 0 (y + 1) wid hgt robots
  else if (y >= hgt) then let () = print_endline "" in 
                         let () = flush stdout in ()                          
  else if x = 0 then let () =  print_endline "" in
    let hr = List.length (has_robot x y robots) in
    match hr with 
    | 0 -> let () = (Printf.printf ".") in show_grid2 (x + 1) y wid hgt robots 
    | _ -> let () = (Printf.printf "%d" hr) in 
           show_grid2 (x + 1) y wid hgt robots 
  else let hr = List.length (has_robot x y robots) in
    match hr with 
    | 0 -> let () = (Printf.printf ".") in show_grid2 (x + 1) y wid hgt robots 
    | _ -> let () = (Printf.printf "%d" hr) in 
           show_grid2 (x + 1) y wid hgt robots 
;;

(* fix show_grid x > wid to x >= wid  *)

let rec foo = (fun () -> show_grid 11 7 ex_vals);;

(* if we run >foo ()  we get a pretty grid 
shows there are 2 robots at 4 0 in the examples 
1.12........
............
............
......11.11.
1.1.........
.........1..
.......1....
............
*)
 
(* 
problem with ocaml is where are the libraries ? 
what is the underlying base system  
how make own libraries , where do we put them so they can be used ??

have a look at single robot example 

*)

let s_val = [Robot(2,4,2,-3)] ;;

let rec foo2 = (fun () -> show_grid 11 7 s_val);;

(*
step takes a robot and advances it one second 
   x2 = x + vx 
   y2 = y + vy  
 in a sense make a new robot 
how does the robot teleport once its off the board 
in order to teleport we need to know the size of the board 
 using zero based indexes too 
*)

let rec teleport (wid: int) (hgt:int) (r:robot) : robot = 
  match r with 
  | Robot(x,y,vx,vy) -> let x2 = x + vx in
                        let y2 = y + vy in
                        Robot(x2 % wid, y2 % hgt, vx ,vy) ;;

(* hard coded width of board to 11 , height of board to 7  *)
(*
let rec step xs = List.map ~f:(fun r -> teleport 11 7 r) xs ;;
*)
let rec step (wid:int) (hgt:int) xs = List.map ~f:(fun r -> teleport wid hgt r) xs ;;

let rec repeat fn xs n = if n > 0 then let ys = fn xs in repeat fn ys (n - 1) 
                               else xs ;;

(* show the development of the board as the robot moves across the board  *)
let rec foo3 n = (fun () -> repeat (fun xs -> let () = show_grid 11 7 xs in step 11 7 xs) s_val n );;
(* we can see the next 3 steps we do 
      >  foo3 5 ()
 *)

(*
(* specialised version specific to s_val we could have  *)
let rec after n = (fun () -> let result = repeat (fun xs -> step xs) s_val n in
                             let () = show_grid 11 7 result in 
                             result );;
*)

(* again more hard coded 11 7 width height  *)
(*
let rec after n robots  = (fun () -> let result = repeat (fun xs -> step xs) robots n in
                             let () = show_grid 11 7 result in 
                             result );;
*)

let rec after wid hgt n robots  = (fun () -> let result = repeat (fun xs -> step wid hgt xs) robots n in
                             let () = show_grid wid hgt result in 
                             result );;


let after_silent wid hgt n robots () = 
    repeat (fun xs -> step wid hgt xs) robots n 
                             



(*
now the updated after routine we do 
> after 11 7 100 ex_vals () ;;

......2..1.
...........
1..........
.11........
.....1.....
...12......
.1....1....
- : robot list =
[Robot (3, 5, 3, -3); Robot (5, 4, -1, -3); Robot (9, 0, -1, 2);
 Robot (4, 5, 2, -1); Robot (1, 6, 1, 3); Robot (1, 3, -2, -2);
 Robot (6, 0, -1, -3); Robot (2, 3, -1, -2); Robot (0, 2, 2, 3);
 Robot (6, 0, -1, 2); Robot (4, 5, 2, -3); Robot (6, 6, -3, -3)]

lets see the big puzzle after 100 
> after 101 103 100 vals () ;;

*)

(* use emacs with a basic M-x shell , then all emacs advantages we can do . *)

                               

(* if we want to see robots after 0 time steps , ie the initial configuration we can do 
> after 0 ex_vals () ;;

after 1 time step we can do 
> after 1 ex_vals () ;; 

101 wide , 103 high split into quadrants , ignoring the middle 
width : 50 left -  1 middle - 50 right  : total of 101 wide 
height : 51 upper - 1 middle - 51 lower : total of 103 high 
split it into quadrants , find number of 
*)

(*

here running through quadrant each time we have to re-compute the whole thing 100 times 

let nw_quadrant wid hgt vals = 
                  let runs = 100 in 
                  let robs = after wid hgt runs vals () in
                  let top_left = List.filter robs ~f:(fun x -> match x with 
                      | Robot(ax,bx,_,_) -> if (ax < (wid / 2)) && (bx < (hgt / 2)) then true
                        else false) in
                  top_left ;;

let ne_quadrant wid hgt vals = 
                  let runs = 100 in 
                  let robs = after wid hgt runs vals () in
                  let top_right = List.filter robs ~f:(fun x -> match x with 
                      | Robot(ax,bx,_,_) -> if (ax > (wid / 2)) && (bx < (hgt / 2)) then true
                        else false) in
                  top_right ;;

let se_quadrant wid hgt vals = 
                  let runs = 100 in 
                  let robs = after wid hgt runs vals () in
                  let bot_right = List.filter robs ~f:(fun x -> match x with 
                      | Robot(ax,bx,_,_) -> if (ax > (wid / 2)) && (bx > (hgt / 2)) then true
                        else false) in
                  bot_right ;;

let sw_quadrant wid hgt vals = 
                  let runs = 100 in 
                  let robs = after wid hgt runs vals () in
                  let bot_left = List.filter robs ~f:(fun x -> match x with 
                      | Robot(ax,bx,_,_) -> if (ax < (wid / 2)) && (bx > (hgt / 2)) then true
                        else false) in
                  bot_left ;;


let quadrant wid hgt vals = 
  let n1 = List.length (nw_quadrant wid hgt vals) in
  let n2 = List.length (ne_quadrant wid hgt vals) in
  let n3 = List.length (se_quadrant wid hgt vals) in
  let n4 = List.length (sw_quadrant wid hgt vals) in
  n1 * n2 * n3 * n4 ;;
*)


let nw_quadrant wid hgt vals_100 = 
                  List.filter vals_100 ~f:(fun x -> match x with 
                      | Robot(ax,bx,_,_) -> if (ax < (wid / 2)) && (bx < (hgt / 2)) then true
                        else false) ;;

let ne_quadrant wid hgt vals_100 = 
                  List.filter vals_100 ~f:(fun x -> match x with 
                      | Robot(ax,bx,_,_) -> if (ax > (wid / 2)) && (bx < (hgt / 2)) then true
                        else false) ;;

let sw_quadrant wid hgt vals_100 = 
                  List.filter vals_100 ~f:(fun x -> match x with 
                      | Robot(ax,bx,_,_) -> if (ax < (wid / 2)) && (bx > (hgt / 2)) then true
                        else false) ;;


let se_quadrant wid hgt vals_100 = 
                  List.filter vals_100 ~f:(fun x -> match x with 
                      | Robot(ax,bx,_,_) -> if (ax > (wid / 2)) && (bx > (hgt / 2)) then true
                        else false) ;;


let quadrant wid hgt vals = 
  let runs = 100 in 
  let vals_100 = after wid hgt runs vals () in
  let n1 = List.length (nw_quadrant wid hgt vals_100) in
  let n2 = List.length (ne_quadrant wid hgt vals_100) in
  let n3 = List.length (se_quadrant wid hgt vals_100) in
  let n4 = List.length (sw_quadrant wid hgt vals_100) in
  n1 * n2 * n3 * n4 ;;

let part1 = quadrant 101 103 vals;;


(* 
spoilers ...
val part1 : int = 218619120 
*)

(*
--- Part Two ---
During the bathroom break, someone notices that these robots seem awfully similar to ones built and used at the North Pole. If they're the same type of robots, they should have a hard-coded Easter egg: very rarely, most of the robots should arrange themselves into a picture of a Christmas tree.

What is the fewest number of seconds that must elapse for the robots to display the Easter egg?

have we got a graphical display to show this from ocaml ?? 

https://caml.inria.fr/pub/docs/oreilly-book/pdf/chap5.pdf

ocamlmktop -custom -o mytoplevel graphics.cma -cclib \
-L/usr/X11/lib -cclib -lX11
*)
open Graphics ;;

(*  fails on stat.key cannot unify or type check  
let res  = 
   open_graph "" ;
   set_window_title "hello world";
   let stat = wait_next_event [Key_pressed] in
   if stat.key = 'f' then true else false 
*)

let width = 900
let height = 900
let s_width = 8 
let s_height = 8 
let off_x = 50 
let off_y = 50 


let ()  = 
   open_graph "" ;
   resize_window width height ;
   set_window_title "hello world"

(* flip vertical because grid expecting 0 0 to be top left , 
 whereas graphics coords 0 0 on bottom left  according to docs 
*)
let draw_blank_square (x:int) (y:int) = 
   set_color Graphics.black;
   fill_rect (x * s_width + off_x) (height - (y * s_height) - off_y) s_width s_height
  
let draw_filled_square (x:int) (y:int) = 
   set_color Graphics.yellow;
   fill_rect (x * s_width + off_x) (height - (y * s_height) - off_y) s_width s_height


(*
let rec viz_grid (wid:int) (hgt:int) (robots : robot list) = 
     viz_grid2 0 0 wid hgt robots 
and
 viz_grid2 (x:int) (y:int) (wid:int) (hgt:int) (robots : robot list) = 
  if (x >= wid) then viz_grid2 0 (y + 1) wid hgt robots
  else if (y >= hgt) then let () = print_endline "" in 
                         let () = flush stdout in ()                          
  else if x = 0 then let () =  print_endline "" in
    let hr = List.length (has_robot x y robots) in
    match hr with 
    | 0 -> let () = draw_blank_square x y in viz_grid2 (x + 1) y wid hgt robots 
    | _ -> let () = (Printf.printf "%d" hr) in 
           viz_grid2 (x + 1) y wid hgt robots 
  else let hr = List.length (has_robot x y robots) in
    match hr with 
    | 0 -> let () = (Printf.printf ".") in viz_grid2 (x + 1) y wid hgt robots 
    | _ -> let () = (Printf.printf "%d" hr) in 
           viz_grid2 (x + 1) y wid hgt robots 
*)


(*
let rec viz_grid (wid:int) (hgt:int) (robots : robot list) = 
     viz_grid2 0 0 wid hgt robots 
and
 viz_grid2 (x:int) (y:int) (wid:int) (hgt:int) (robots : robot list) = 
  if (x >= wid) then viz_grid2 0 (y + 1) wid hgt robots
  else if (y >= hgt) then let () = print_endline "" in 
                         let () = flush stdout in ()                          
  else if x = 0 then let () =  print_endline "" in
    let hr = List.length (has_robot x y robots) in
    match hr with 
    | 0 -> let () = draw_blank_square x y in 
                    viz_grid2 (x + 1) y wid hgt robots 
    | _ -> let () = draw_filled_square x y in 
                    viz_grid2 (x + 1) y wid hgt robots 
  else let hr = List.length (has_robot x y robots) in
    match hr with 
    | 0 -> let () = draw_blank_square x y in
                    viz_grid2 (x + 1) y wid hgt robots 
    | _ -> let () = draw_filled_square x y in 
           viz_grid2 (x + 1) y wid hgt robots 
*)


let rec viz_grid (robots : robot list) = 
   match robots with 
   | [] -> ()
   | Robot(ax,ay,vx,vy) :: rest -> 
        draw_filled_square ax ay ;
        viz_grid rest 




(* also key pressed does not understand arrow keys ?? only qwerty keys *)
(*
let res  = 
   open_graph "" ;
   set_window_title "hello world";
   let stat = wait_next_event [Key_pressed] in
   match stat.key with 
   | 'f' -> stat.key
   | _ ->  stat.key

  a key = decrease n
  d key = increase n 

  any other key escapes the event loop 
*)
(*
let rec event_loop n () = 
   set_window_title ("hello world " ^ (string_of_int n)) ;
   (* redraw the screen  *)
   set_color Graphics.black;
   fill_rect 0 0 width height;
   let wid = 101 in
   let hgt = 103 in 
   let robots = after_silent wid hgt n vals () in 
   viz_grid wid hgt robots; 
 
 (*
   set_color Graphics.yellow;
   fill_rect 0 0 100 100;
   *)

  (* wait for key press  *)
   let stat = wait_next_event [Key_pressed] in
   match stat.key with 
   | 'a' -> let n2 = max 0 (n - 1) in event_loop n2 ()            
   | 'd' -> event_loop (n + 1) ()
   | _ -> ()
*)


(*
use vals as big external original robot configuration 
when user pressed a ie to go backward in computation - we just recompute everything again 
when user presses d to advance we just compute from current robots 
*)
let rec event_loop2 n robots () = 
   set_window_title ("hello world " ^ (string_of_int n)) ;
   (* redraw the screen  *)
   set_color Graphics.black;
   fill_rect 0 0 width height;
 
   let wid = 101 in
   let hgt = 103 in 
   (* let robots = after_silent wid hgt n vals () in  *)
   viz_grid robots; 
 
 (*
   set_color Graphics.yellow;
   fill_rect 0 0 100 100;
   *)

  (* wait for key press  *)
   (* let stat = wait_next_event [Key_pressed] in 
   match stat.key with
   *)
   Unix.sleepf 0.1 ; 
   match 'd' with 
   | 'a' -> let n2 = max 0 (n - 1) in 
                   (* no change *)
            if n = n2 then event_loop2 n robots () 
                              (* after_silent wid hgt n robots *)
            else let next_vals = after_silent wid hgt n2 vals () in 
                     event_loop2 n2 next_vals () 
   | 'd' -> let next_vals = after_silent wid hgt 1 robots () in 
             event_loop2 (n + 1) next_vals () 
   | _ -> ()

(* start up the event loop with initial robot positions  *)
let () = event_loop2 0 vals ()

(* shut down window  *)
let () = 
   Graphics.close_graph () 






(*
(* assume its localhost , providing a non empty string sends it  *)
let width = 600 ;;
let height = 600 ;;
let () = Graphics.open_graph "";;
let () = Graphics.set_window_title "advent code 2024 day 14";;
let () = Graphics.set_color Graphics.black ;;
let () = Graphics.resize_window width height;;
let () = Graphics.clear_graph () ;;
let () = Graphics.fill_rect 0 0 width height;;
let () = Graphics.set_color Graphics.yellow ;;
let () = Graphics.fill_rect 0 0 100 100;;
let rec event_loop () = let stat = wait_next_event [Key_pressed] in
                    Printf.printf "%c" stat.key 
                    if stat.key = 'f' then () else event_loop () ;;

let () = event_loop () ;;
                    
(*
let event_loop = 
         if stat.key = 'f' then Graphics.close_graph
         else event_loop ;;
*)

let () = 
   Graphics.close_graph () ;;
*)




(*
setup the editor for ocaml o m g - truely awful 
reload time on code is a disaster - have to keep typing #use "fun.ml";; 

emacs setup with ctrl + up arrow to get previously entered code is truely awful - not like 
slime emacs when can just get last s expression entered 
*)

(* if i return an option type Some robot or None ... or a list of robots , or an empty list  
*)


(* forgetting that has_robot is recursive - need to put let rec  *)

(*
let rec has_robot (x:int) (y:int) robots : bool = 
  match robots with 
  | (Some (Robot (a,b,c,d))) :: rest -> if x = a && y = b then true else has_robot x y (List.tl robots)
  | _ -> false ;;
*)

(* fixed a show grid bug that made grid appear wider and longer than it should , off by one error *)

(* 
should we use an array for grid - rather than lists and traversing lists we can just do an 
array look up 
xy = x + wid * y 
for xy zero based indexes 
if y 0 then just x 
if y > 0 then x + wid * y , wid is next row as x only ranges from 0 to wid-1 so seems correct 

array of robots but doesnt seem like that would be an advantage 

loop over the robots rather than the entire grid as there are 101 * 103 squares but only 500 robots

*)
