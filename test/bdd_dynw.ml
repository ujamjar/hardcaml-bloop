(* dynamic weight assignment example *)

open HardCamlBloop
open Expr

let a, b, c, d, e = var "a", var "b", var "c", var "d", var "e"

let g1 = a &: b
let g2 = (c &: d) &: b
let f = g1 |: g2
let g = d ^: e

open Printf
open Bddx

let show bdd = 
  let () = Bdd.print_to_dot bdd "temp.dot" in
  ignore @@ Unix.system "dot -Tx11 temp.dot"

(* show with ordering heuristic *)
let order = dynamic_weight_assignment [[f];[g]]
let () = M.iter (fun v i -> printf "result: %s{%i}\n" (string_of_t v) i) order
let fbdd = of_expr order f
let gbdd = of_expr order g

let () = show fbdd
let () = show gbdd

(* without ordering *)
let order = vars_of_signal [f;g]
let fbdd = of_expr order f
let gbdd = of_expr order g

let () = show fbdd
let () = show gbdd

