(* interface to minisat *)

module M = Map.Make(struct type t = Expr.t let compare = compare end)

type vec_result = string * Sattools.Lbool.t array
type next_sat_result = unit -> sat_result
and  sat_result = (vec_result list * next_sat_result) Sattools.Result.t

let time verbose = 
  if verbose then
    (fun s f a ->
      let time = Unix.time () in
      let r = f a in
      let time = Unix.time () -. time in
      Printf.printf "%s: %.2f sec\n%!" s time;
      r)
  else (fun _ f a -> f a)

exception Sat_signal_width_not_1
exception Sat_banned_expr_too_complex

type solver_intf = 
  {
    destroy : unit -> unit;
    add_clause : int list -> unit;
    solve : unit -> unit Sattools.Result.t;
    model : int -> Sattools.Lbool.t;
  }

let make_solver ?(solver="dimacs-mini") ?(verbose=false) s = 
    let open Gates.Tseitin in
    let cnf = time verbose "tseitin" of_expr s in
    let satsolver = 
      let s = Sattools.Libs.get_solver solver in
      let module S = (val s : Sattools.Libs.Solver) in
      let s = S.create () in
      {
        destroy = (fun () -> S.destroy s);
        add_clause = (fun c -> S.add_clause s c);
        solve = (fun () -> S.solve s);
        model = S.model s;
      }
    in
    let rec clauses = function
      | Var _ 
      | Ref _ -> ()
      | Term(x,a) ->
        List.iter satsolver.add_clause x;
        List.iter clauses a
    in
    let () = time verbose "clauses" clauses cnf.terms in
    let () = satsolver.add_clause [ cnf.top_term ] in
    cnf, satsolver

let soln_vecs x = 
  let error () = failwith "report error" in
  let rec sort l = 
    match l with
    | [] -> []
    | (Expr.Var(_,n,_),_,_) :: _ ->
        let x,y = 
          List.partition 
            (function (Expr.Var(_,n',_),_,_) when n=n' -> true | _ -> false)
            l
        in
        x :: sort y
    | _ -> error ()
  in
  let compare i j = 
    match i,j with
    | (Expr.Var(_,_,i),_,_), (Expr.Var(_,_,j),_,_) -> - (compare i j) 
    | _ -> error ()
  in
  let f = List.map (List.sort compare) @@ sort x in
  let vec x = 
    match x with
    | [] -> "unknown", [||]
    | (Expr.Var(_,n,i),_,_) :: _ -> 
        let a = Array.init (i+1) (fun _ -> `u) in
        List.iter (function (Expr.Var(_,_,i),_,v) -> a.(i) <- v | _ -> error()) x;
        n, a
    | _ -> error()
  in
  List.map vec f 

let of_expr ?(solver="dimacs-mini") ?(verbose=false) ?(banned=[]) s = 
  let inputs, satsolver = 
    let open Gates.Tseitin in
    let cnf, satsolver = make_solver ~solver ~verbose s in
    let add_banned_solutions () = 
      let open Expr in
      let f b = 
        let rec eval e = 
          match e with
          | T | F | Var _ -> e
          | And(_, e0, e1) -> Gates.Basic_gates_opt.(eval e0 &. eval e1)
          | Or(_, e0, e1)  -> Gates.Basic_gates_opt.(eval e0 |. eval e1)
          | Xor(_, e0, e1) -> Gates.Basic_gates_opt.(eval e0 ^. eval e1)
          | Not(_, e0)     -> Gates.Basic_gates_opt.(~. (eval e0))
        in
        match eval b with
        | T | F -> None
        | Var(id,_,_) -> Some(- (Expr.Umap.find id cnf.map))
        | Not(_,Var(id,_,_)) -> Some(Expr.Umap.find id cnf.map)
        | _ -> raise Sat_banned_expr_too_complex
      in
      let map_f ban = 
        List.map (function Some(x) -> x | None -> failwith "") @@ 
        List.filter ((<>)None) @@ 
        List.map f ban 
      in
      List.iter (fun ban -> satsolver.add_clause (map_f ban)) banned
    in
    add_banned_solutions ();
    cnf.vars, satsolver
  in
  let get_soln_value idx = 
    match satsolver.model idx with
    | exception _ -> `u (* if it happens? *)
    | _ as x -> x
  in
  let get_soln () = 
    List.sort (fun (a,_,_) (b,_,_) -> compare a b) @@
    Gates.Tseitin.Vmap.fold 
      (fun idx v l -> (v, idx, get_soln_value idx) :: l) 
      inputs [] 
  in
  let add_solution_clause soln sat =
    if soln = [] then ()
    else
      let rec f l = function
        | [] -> l
        | (_, idx, `t)::t -> f ((-idx) :: l) t
        | (_, idx, `f)::t -> f (idx :: l) t
        | _ :: t -> f l t
      in
      let clause = f [] soln in
      sat.add_clause clause
  in
  let rec next soln () = 
    add_solution_clause soln satsolver;
    let soln = time verbose "solve" (satsolver.solve) () in
    match soln with
    | `sat () ->
      let soln = get_soln () in
      `sat (soln_vecs soln, next soln)
    | `unsat -> `unsat
  in
  next [] ()

let of_signal ?(solver="dimacs-mini") ?(verbose=false) ?(banned=[]) s = 
  if Gates.Comb.width s <> 1 then raise Sat_signal_width_not_1
  else of_expr ~solver ~verbose ~banned (List.hd s)

open Printf

let string_of_vec_result ?(unknown='_') (n, v) = 
  let len = Array.length v in
  String.init len 
    (fun i -> 
      match v.(len-i-1) with
      | `t -> '1'
      | `f -> '0'
      | `u -> unknown)


let report_soln vecs = 
  List.iter (fun (n,a) -> printf "%s: %s\n" n (string_of_vec_result (n,a))) vecs

let report = function
  | `unsat -> printf "UNSAT\n"
  | `sat (x,_) -> printf "SAT\n"; report_soln x

