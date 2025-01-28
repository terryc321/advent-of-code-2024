
(* repeat a function applied to a list of things n times where 0 means no times , just return result *)
let rec repeat fn xs n = if n > 0 then let ys = fn xs in repeat fn ys (n - 1) 
                               else xs ;;

