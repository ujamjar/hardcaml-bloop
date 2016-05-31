(* interface to minisat *)

open Minisat
open Gates.Consistency

module M = Map.Make(struct type t = Expr.t let compare = compare end)

type sat_result = [ `sat of (Expr.t * [ `t | `f | `u ]) list | `unsat ]

let cleanup ~cleaner x = 
  if not cleaner then `expr x
  else remove_constants x

let eval ?(cleaner=true) cons = 

  match cleanup ~cleaner cons with
  | `unsat -> `unsat
  | `expr cons ->
    let sat = new solver in

    (* find all variable instances *)
    let add v m = M.add v sat#new_var m in
    let var_map = Svar.fold add cons.input_vars M.empty in
    let var_map = Svar.fold add cons.temp_vars var_map in

    (* create clauses *)
    let () = 
      Sexpr.iter 
        (fun sums ->
          let clause = Svar.fold 
            (fun s l ->
              match s with
              | Expr.Var(_) as x -> pos_lit (M.find x var_map) :: l
              | Expr.Not(Expr.Var(_) as x) -> neg_lit (M.find x var_map) :: l
              | _ -> failwith "Not a literal")
            sums
            []
          in
          sat#add_clause clause)
        cons.exprs
    in
    let () = sat#add_clause [ pos_lit (M.find cons.out_var var_map) ] in

    match sat#solve with
    | SAT -> `sat (Svar.fold (fun v l -> 
        (v, 
          (match sat#value_of (M.find v var_map) with
          | True -> `t
          | False -> `f
          | Unknown -> `u)) :: l) cons.input_vars [])
    | UNSAT -> `unsat 

exception Sat_signal_width_not_1

let of_signal ?(cleaner=true) s = 
  if Gates.Comb.width s <> 1 then raise Sat_signal_width_not_1
  else
    let c = of_signal s in
    match cleanup ~cleaner (List.hd c) with
    | `unsat -> `unsat
    | `expr c -> eval ~cleaner:false c (* dont clean again *)

open Printf

let report = function
  | `unsat -> printf "UNSAT\n"
  | `sat(x) ->
      printf "SAT:\n";
      List.iter (function 
        | (Expr.Var(n,i),`t) -> printf "%s[%i] = 1\n" n i
        | (Expr.Var(n,i),`f) -> printf "%s[%i] = 0\n" n i
        | (Expr.Var(n,i),`u) -> printf "%s[%i] = X\n" n i
        | _ -> failwith "bad result") x

