(* Multiplcation was very slow with the old sat interface.
   The new one is ~2 orders of magnitude faster.

   Also demos iterative solution finding. *)

open HardCamlBloop
open Gates.Comb

let a, b = input "a" 4, input "b" 4
let c = (((a +: b) *: (a -: b)) ==:. 3)

let f a b = ((((a + b) land 15) * ((a - b) land 15)) land 255) = 3

(* $ let soln = solve c;;
   $ let soln = next ();;
   $ let soln = next ();;
   ... *)

let solve c = 
  let soln = Sat.of_signal ~sharing:true c in
  Sat.report soln;
  soln

let next soln = 
  match soln with
  | `unsat -> failwith "no more solutions"
  | `sat(_,next) -> 
    let soln = next () in
    Sat.report soln;
    soln

(* Automatically search for all solutions *)

let all_solns c = 
  let rec f soln solns = 
    match soln with
    | `unsat -> solns
    | `sat(soln, next) -> f (next()) (soln::solns)
  in
  f (Sat.of_signal c) []

let solns = all_solns c
let () = List.iter Sat.report_soln solns

