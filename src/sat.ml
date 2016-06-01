(* interface to minisat *)

module M = Map.Make(struct type t = Expr.t let compare = compare end)

type model = Expr.t * int * [ `t | `f | `u ]
type next_sat_result = unit -> sat_result
and  sat_result = [ `sat of model list * next_sat_result | `unsat ]
type solver = [ `crypto | `minisat ]

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

module Minisat_solver = struct
  open Minisat
  class t = object(self)
    val solver = new solver
    val mutable vars = [||]
    method init_vars nterms = vars <- Array.init nterms (fun _ -> solver#new_var)
    method get_var i = 
      if i>0 then pos_lit vars.(i-1) 
      else neg_lit vars.((-i)-1)
    method add_clause c = solver#add_clause (List.map (self#get_var) c)
    method solve : [`sat|`unsat] = 
      match solver#solve with
      | SAT -> `sat
      | UNSAT -> `unsat
    method model i : [`t|`f|`u] = 
      match solver#value_of vars.(i-1) with
      | True -> `t
      | False -> `f 
      | Unknown -> `u
    method print_stats = solver#print_stats
  end
end

module Cryptominisat_solver = struct
  open Cryptominisat 
  class t = 
    let solver = Cryptominisat.L.create () in
    let vec_of_lits v l = 
      L.Vec.clear v;
      List.iter 
        (fun i ->
          let sgn = i < 0 in
          let i = abs i in
          if i = 0 then raise Cryptominisat_zero_literal 
          else L.Vec.push_back v (i-1) sgn)
        l
    in
    let vec = L.Vec.create () in
    let add_clause c = 
      vec_of_lits vec c;
      L.add_clause solver vec
    in
  object(self)
    method init_vars nterms = Cryptominisat.L.new_vars solver nterms
    method get_var (i:int) = i
    method add_clause c = add_clause c
    method solve : [`sat|`unsat] = 
      match L.solve solver with
      | T -> `sat
      | F -> `unsat
      | U -> failwith "cryptominisat failed"
    method model idx : [`t|`f|`u] = 
      match L.get_model solver (idx-1) with
      | T -> `t
      | F -> `f 
      | U -> `u
    method print_stats = L.print_stats solver
  end
end

let make_solver ?(solver=`minisat) ?(verbose=false) ?(sharing=true) s = 
    let open Gates.Tseitin in
    let cnf = time verbose "tseitin" (of_expr ~sharing) s in
    let satsolver = 
      match solver with
      | `crypto -> new Cryptominisat_solver.t
      | `minisat -> new Minisat_solver.t
    in
    let () = time verbose "init vars" satsolver#init_vars cnf.nterms in
    let rec clauses = function
      | Var _ (*id -> sat#add_clause [ get_var id ]*)
      | Ref _ -> ()
      | Term(x,a) ->
        List.iter satsolver#add_clause x;
        List.iter clauses a
    in
    let () = time verbose "clauses" clauses cnf.terms in
    let () = satsolver#add_clause [ cnf.nterms ] in
    cnf.vars, satsolver

let of_expr ?(solver=`minisat) ?(verbose=false) ?(sharing=true) s = 
  let inputs, satsolver = make_solver ~solver ~verbose ~sharing s in
  let get_soln_value idx = 
    match satsolver#model idx with
    | exception _ -> `u (* if it happens? *)
    | _ as x -> x
  in
  let get_soln () = 
    List.sort (fun (a,_,_) (b,_,_) -> compare a b) @@
    Hashtbl.fold 
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
      sat#add_clause clause
  in
  let rec next soln () = 
    add_solution_clause soln satsolver;
    let soln = time verbose "solve" (fun () -> satsolver#solve) () in
    let () = if verbose then satsolver#print_stats in
    match soln with
    | `sat ->
      let soln = get_soln () in
      `sat (soln, next soln)
    | `unsat -> `unsat
  in
  next [] ()

let of_signal ?(solver=`minisat) ?(verbose=false) ?(sharing=true) s = 
  if Gates.Comb.width s <> 1 then raise Sat_signal_width_not_1
  else of_expr ~solver ~verbose ~sharing (List.hd s)

open Printf

let report_soln x = 
  let error () = failwith "report error" in
  let rec sort l = 
    match l with
    | [] -> []
    | (Expr.Var(n,_),_,_) :: _ ->
        let x,y = 
          List.partition 
            (function (Expr.Var(n',_),_,_) when n=n' -> true | _ -> false)
            l
        in
        x :: sort y
    | _ -> error ()
  in
  let compare i j = 
    match i,j with
    | (Expr.Var(_,i),_,_), (Expr.Var(_,j),_,_) -> - (compare i j) 
    | _ -> error ()
  in
  let f = List.map (List.sort compare) @@ sort x in
  let vec x = 
    match x with
    | [] -> "unknown", [||]
    | (Expr.Var(n,i),_,_) :: _ -> 
        let a = Array.init (i+1) (fun _ -> `u) in
        List.iter (function (Expr.Var(_,i),_,v) -> a.(i) <- v | _ -> error()) x;
        n, a
    | _ -> error()
  in
  let vecs = List.map vec f in
  List.iter 
    (fun (n,a) ->
      printf "%s: " n;
      for i=Array.length a - 1 downto 0 do
        printf "%c"
          (match a.(i) with
          | `t -> '1'
          | `f -> '0'
          | `u -> '_')
      done;
      printf "\n")
    vecs

let report = function
  | `unsat -> printf "UNSAT\n"
  | `sat (x,_) -> printf "SAT\n"; report_soln x

module Old = struct

  open Gates.Consistency
  open Minisat

  let cleanup ~cleaner x = 
    if not cleaner then `expr x
    else remove_constants x

  let eval ?(cleaner=false) cons = 

    match cleanup ~cleaner cons with
    | `unsat -> `unsat
    | `expr cons ->
      let sat = new solver in

      (* find all variable instances *)
      let var_map = time true "create vars" (fun () ->
        let add v m = M.add v sat#new_var m in
        let var_map = Svar.fold add cons.input_vars M.empty in
        let var_map = Svar.fold add cons.temp_vars var_map in
        var_map) ()
      in

      (* create clauses *)
      let clauses = time true "generate clauses" (fun () ->
        Sexpr.fold
          (fun sums lst ->
            let clause = Svar.fold 
              (fun s l ->
                match s with
                | Expr.Var(_) as x -> pos_lit (M.find x var_map) :: l
                | Expr.Not(Expr.Var(_) as x) -> neg_lit (M.find x var_map) :: l
                | _ -> failwith "Not a literal")
              sums
              []
            in
            clause::lst)
          cons.exprs []) ()
      in
      let () = time true "add clauses" (fun () -> List.iter sat#add_clause clauses) () in

      let () = sat#add_clause [ pos_lit (M.find cons.out_var var_map) ] in
      let () = sat#print_stats in

      match time true "solve" (fun () -> sat#solve) () with
      | SAT -> `sat 
          ((Svar.fold (fun v l -> 
            (v, 0,
              (match sat#value_of (M.find v var_map) with
              | True -> `t
              | False -> `f
              | Unknown -> `u)) :: l) cons.input_vars []), 
          (fun () -> failwith "not implemented"))
      | UNSAT -> `unsat 

  let of_signal ?(cleaner=false) s = 
    if Gates.Comb.width s <> 1 then raise Sat_signal_width_not_1
    else
      let c = time true "tseitin" of_signal s in
      match time true "cleanup" (fun () -> cleanup ~cleaner (List.hd c)) () with
      | `unsat -> `unsat
      | `expr c -> eval ~cleaner:false c (* dont clean again *)

end
