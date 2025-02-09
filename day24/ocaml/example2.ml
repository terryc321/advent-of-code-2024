
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
   

(* z : (z00 z01 z02 z03 z04 z05 z06 z07 z08 z09 z10 z11 z12) *)

(* declarations *)
let bfw : int option ref = ref None;;
let bqk : int option ref = ref None;;
let djm : int option ref = ref None;;
let ffh : int option ref = ref None;;
let fgs : int option ref = ref None;;
let frj : int option ref = ref None;;
let fst : int option ref = ref None;;
let gnj : int option ref = ref None;;
let hwm : int option ref = ref None;;
let kjc : int option ref = ref None;;
let kpj : int option ref = ref None;;
let kwq : int option ref = ref None;;
let mjb : int option ref = ref None;;
let nrd : int option ref = ref None;;
let ntg : int option ref = ref None;;
let pbm : int option ref = ref None;;
let psh : int option ref = ref None;;
let qhw : int option ref = ref None;;
let rvg : int option ref = ref None;;
let tgd : int option ref = ref None;;
let tnw : int option ref = ref None;;
let vdt : int option ref = ref None;;
let wpb : int option ref = ref None;;
let x00 : int option ref = ref None;;
let x01 : int option ref = ref None;;
let x02 : int option ref = ref None;;
let x03 : int option ref = ref None;;
let x04 : int option ref = ref None;;
let y00 : int option ref = ref None;;
let y01 : int option ref = ref None;;
let y02 : int option ref = ref None;;
let y03 : int option ref = ref None;;
let y04 : int option ref = ref None;;
let z00 : int option ref = ref None;;
let z01 : int option ref = ref None;;
let z02 : int option ref = ref None;;
let z03 : int option ref = ref None;;
let z04 : int option ref = ref None;;
let z05 : int option ref = ref None;;
let z06 : int option ref = ref None;;
let z07 : int option ref = ref None;;
let z08 : int option ref = ref None;;
let z09 : int option ref = ref None;;
let z10 : int option ref = ref None;;
let z11 : int option ref = ref None;;
let z12 : int option ref = ref None;;


(* assignments  *)
x00 := Some 1;;
x01 := Some 0;;
x02 := Some 1;;
x03 := Some 1;;
x04 := Some 0;;
y00 := Some 1;;
y01 := Some 1;;
y02 := Some 1;;
y03 := Some 1;;
y04 := Some 1;;

(* operations  *)
let rec foo () = 
let zs = [z00;z01;z02;z03;z04;z05;z06;z07;z08;z09;z10;z11;z12] in 
let undet = List.filter (fun x -> match (!x) with | Some _ -> false | _ -> true) zs in 
if undet = [] then zs 
else ( ixor   ntg   fgs   mjb ;
 ior   y02   x01   tnw ;
 ior   kwq   kpj   z05 ;
 ior   x00   x03   fst ;
 ixor   tgd   rvg   z01 ;
 ior   vdt   tnw   bfw ;
 iand   bfw   frj   z10 ;
 ior   ffh   nrd   bqk ;
 iand   y00   y03   djm ;
 ior   y03   y00   psh ;
 ior   bqk   frj   z08 ;
 ior   tnw   fst   frj ;
 iand   gnj   tgd   z11 ;
 ixor   bfw   mjb   z00 ;
 ior   x03   x00   vdt ;
 iand   gnj   wpb   z02 ;
 iand   x04   y00   kjc ;
 ior   djm   pbm   qhw ;
 iand   nrd   vdt   hwm ;
 iand   kjc   fst   rvg ;
 ior   y04   y02   fgs ;
 iand   y01   x02   pbm ;
 ior   ntg   kjc   kwq ;
 ixor   psh   fgs   tgd ;
 ixor   qhw   tgd   z09 ;
 ior   pbm   djm   kpj ;
 ixor   x03   y03   ffh ;
 ixor   x00   y04   ntg ;
 ior   bfw   bqk   z06 ;
 ixor   nrd   fgs   wpb ;
 ixor   frj   qhw   z04 ;
 ior   bqk   frj   z07 ;
 ior   y03   x01   nrd ;
 iand   hwm   bqk   z03 ;
 ixor   tgd   rvg   z12 ;
 ior   tnw   pbm   gnj ;
foo());; 

let solution = bin2dec (foo ()) 0 1 ;;