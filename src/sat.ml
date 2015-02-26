(* interface to minisat *)

open Minisat
open Gates.Consistency

module M = Map.Make(struct type t = Expr.t let compare = compare end)

let run cons = 

  let sat = new solver in

  (* find all variable instances *)
(*  let map = 
    Sexpr.fold 
      (fun sums map ->
        Svar.fold 
          (fun var map ->
            try (ignore @@ M.find var map; map)
            with Not_found -> 
              let sat_var = sat#new_var in
              M.add var sat_var map) sums map)
    cons.exprs
    M.empty
  in
*)
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

