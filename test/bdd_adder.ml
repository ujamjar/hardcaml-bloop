(* BDD of a 4 bit adder.
 * We know the "right" solution here, so we can compare to it.
 * Results?  Heuristic is much better than random, not as good as the best solution.
 * Overall, not too bad. *)

open HardCamlBloop
open Gates.Comb

let a, b = input "a" 4, input "b" 4

let f = a +: b

open Bddx

let show bdd = 
  let () = Bdd.print_to_dot bdd "temp.dot" in
  ignore @@ Unix.system "dot -Tx11 temp.dot"

(* heuristic ordering *)
let order = dynamic_weight_assignment [f]
let bdd = of_signal order f
let () = List.iter show bdd

(* some random(ish) order  *)
let order = vars_of_signal f
let bdd = of_signal order f
let () = List.iter show bdd

(* best order (constructed manually) *)
let order = List.fold_right (fun (k,v) m -> Expr.M.add k v m) 
  List.(concat (Array.to_list @@ Array.init 4 
    (fun i -> [ List.hd (bit a i),(i*2+1); List.hd (bit b i),(i*2+2)])))
  Expr.M.empty
let bdd = of_signal order f
let () = List.iter show bdd


