(* Multiplcation was very slow with the old sat interface.
   The new one is ~2 orders of magnitude faster.

   Also demos iterative solution finding. *)

open HardCamlBloop
open Gates.Comb

let verbose = ref false
let solver = ref "minisat"
let no_sharing = ref false
let max_solns = ref 100
let () = Arg.parse 
  [
    "-v", Arg.Set verbose, "print stats";
    "-s", Arg.Set_string solver, "choose solver (minisat* or cryptominisat)";
    "-d", Arg.Set no_sharing, "disable sharing optimisation";
    "-m", Arg.Set_int max_solns, "maximum number of solutions to find";
  ]
  (fun s -> raise (Arg.Help "invalid positional parameter"))
  "sat multiplier test"
let verbose = !verbose
let solver = 
  match !solver with
  | "minisat" | "mini" -> `minisat
  | "crypto" | "cryptominisat" -> `crypto
  | _ -> failwith "invalid solver"
let sharing = not (!no_sharing)

let a, b = input "a" 4, input "b" 4
let c = (((a +: b) *: (a -: b)) ==:. 3)

let f a b = ((((a + b) land 15) * ((a - b) land 15)) land 255) = 3

(* $ let soln = solve c;;
   $ let soln = next ();;
   $ let soln = next ();;
   ... *)

let solve c = 
  let soln = Sat.of_signal ~solver ~verbose ~sharing c in
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
  let rec f cnt soln solns = 
    match soln with
    | `unsat -> solns
    | `sat _ when cnt >= !max_solns -> solns
    | `sat(soln, next) -> f (cnt+1) (next()) (soln::solns)
  in
  f 0 (Sat.of_signal ~solver ~verbose ~sharing c) []

let solns = all_solns c
let () = List.iter Sat.report_soln solns

