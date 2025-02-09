
(* definitions *)
let iand (x : int option ref) (y : int option ref) (z : int option ref) =
  match (!x,!y) with
  |  (None , _) -> ()
  |  (_ , None) -> ()    
  |  (Some 1, Some 1) -> z := Some 1    
  |  _ -> z := Some 0

let ior (x : int option ref) (y : int option ref) (z : int option ref) =
  match (!x,!y) with
  |  (None , _) -> ()
  |  (_ , None) -> ()    
  |  (Some 1, _) -> z := Some 1
  |  (_, Some 1) -> z := Some 1                      
  |  _ -> z := Some 0


let ixor (x : int option ref) (y : int option ref) (z : int option ref) =
  match (!x,!y) with
  |  (None , _) -> ()
  |  (_ , None) -> ()    
  |  (Some 1, Some 0) -> z := Some 1
  |  (Some 0, Some 1) -> z := Some 1                  
  |  _ -> z := Some 0

exception Bin2decException
  
let rec bin2dec xs n s =
  match xs with
  | [] -> n
  | (y :: t) -> (match !y with
      | Some x -> bin2dec t (n + (x * s)) (s * 2)
      | _ -> raise Bin2decException )
   

(* z : (z00 z01 z02) *)

(* declarations *)
let x00 : int option ref = ref None;;
let x01 : int option ref = ref None;;
let x02 : int option ref = ref None;;
let y00 : int option ref = ref None;;
let y01 : int option ref = ref None;;
let y02 : int option ref = ref None;;
let z00 : int option ref = ref None;;
let z01 : int option ref = ref None;;
let z02 : int option ref = ref None;;


(* assignments  *)
x00 := Some 1;;
x01 := Some 1;;
x02 := Some 1;;
y00 := Some 0;;
y01 := Some 1;;
y02 := Some 0;;

(* operations  *)
let rec foo () = 
let zs = [z00;z01;z02] in 
let undet = List.filter (fun x -> match (!x) with | Some _ -> false | _ -> true) zs in 
if undet = [] then zs 
else ( iand   x00   y00   z00 ;
 ixor   x01   y01   z01 ;
 ior   x02   y02   z02 ;
foo());; 

let solution = bin2dec (foo ()) 0 1 ;;