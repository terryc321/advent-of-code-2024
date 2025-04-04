
         
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
   









