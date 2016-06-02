(* interface to minisat *)

module M = Map.Make(struct type t = Expr.t let compare = compare end)

type model = Expr.t * int * [ `t | `f | `u ]
type next_sat_result = unit -> sat_result
and  sat_result = [ `sat of model list * next_sat_result | `unsat ]
type solver = [ 
  | `crypto 
  | `mini
  | `dimacs of [ `crypto | `mini | `pico ]
]

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
    method private get_var i = 
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
    let add_clause solver c = 
      vec_of_lits vec c;
      L.add_clause solver vec
    in
  object(self)
    val solver = Cryptominisat.L.create () 
    method init_vars nterms = Cryptominisat.L.new_vars solver nterms
    method add_clause c = add_clause solver c
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

module Dimacs_solver = struct

  class t solver = object(self)
    val mutable nterms = 0
    val mutable nclauses = 0
    val mutable clauses : int list list = []
    val mutable model : int list = []

    method init_vars n = nterms <- n
    method add_clause c =
      nclauses <- nclauses + 1;
      clauses <- c :: clauses

    method solve : [`sat|`unsat] = 
      let open Printf in

      (* clear the model *)
      model <- [];

      let solver_name = 
        match solver with
        | `crypto -> "cryptominisat4_simple"
        | `mini -> "minisat"
        | `pico -> "picosat"
      in

      (* write cnf *)
      let fin = Filename.temp_file "sat_in_" solver_name in
      let f = open_out fin in
      fprintf f "p cnf %i %i\n" nterms nclauses;
      List.iter (fun term -> 
        List.iter (fun lit -> fprintf f "%i " lit) term;
        fprintf f "0\n") clauses;
      close_out f;
      
      (* output file *)
      let fout = Filename.temp_file "sat_out_" solver_name in
      let () = 
        match solver with 
        | `crypto -> 
          ignore @@ Unix.system(sprintf "%s --verb=0 %s > %s" solver_name fin fout)
        | `mini -> 
          ignore @@ Unix.system(sprintf "%s -verb=0 %s %s" solver_name fin fout)
        | `pico -> 
          ignore @@ Unix.system(sprintf "%s %s > %s" solver_name fin fout)
      in

      (* read output file *)
      let f = open_in fout in
      let result = 
        match input_line f with
        | "SATISFIABLE" | "SAT" | "s SATISFIABLE" -> `sat
        | "UNSATISFIABLE" | "UNSAT" | "s UNSATISFIABLE" -> `unsat
        | _ -> failwith "DIMACS bad output"
        | exception _ -> failwith "DIMACS bad output"
      in
      let () = 
        if result = `sat then 
          let split_char sep str =
            let rec indices acc i =
              try
                let i = succ(String.index_from str i sep) in
                indices (i::acc) i
              with Not_found -> (String.length str + 1) :: acc
            in
            let is = indices [0] 0 in
            let rec aux acc = function
              | last::start::tl ->
                  let w = String.sub str start (last-start-1) in
                  aux (w::acc) (start::tl)
              | _ -> acc
            in
            aux [] is 
          in
          let rec read_result_lines () = 
            match input_line f with
            | _ as line -> 
              let tokens = List.filter ((<>) "") @@ split_char ' ' line in
              (match tokens with
              | "v" :: tl -> model <- List.map int_of_string tl @ model
              | _ as l -> model <- List.map int_of_string l @ model);
              read_result_lines ()
            | exception _ ->
              ()
          in
          read_result_lines ()
      in
      close_in f;
      Unix.unlink fin;
      Unix.unlink fout;
      result

    method model i : [`t|`f|`u] = 
      if i <= 0 then `u
      else if List.mem i model then `t
      else if List.mem (-i) model then `f
      else `u

    method print_stats = ()

  end

end

let make_solver ?(solver=`mini) ?(verbose=false) s = 
    let open Gates.Tseitin in
    let cnf = time verbose "tseitin" of_expr s in
    let satsolver = 
      match solver with
      | `crypto -> new Cryptominisat_solver.t
      | `mini -> new Minisat_solver.t
      | `dimacs(s) -> new Dimacs_solver.t s
    in
    let () = time verbose "init vars" satsolver#init_vars cnf.nterms in
    let rec clauses = function
      | Var _ 
      | Ref _ -> ()
      | Term(x,a) ->
        List.iter satsolver#add_clause x;
        List.iter clauses a
    in
    let () = time verbose "clauses" clauses cnf.terms in
    let () = satsolver#add_clause [ cnf.top_term ] in
    cnf.vars, satsolver

let of_expr ?(solver=`mini) ?(verbose=false) s = 
  let inputs, satsolver = make_solver ~solver ~verbose s in
  let get_soln_value idx = 
    match satsolver#model idx with
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

let of_signal ?(solver=`mini) ?(verbose=false) s = 
  if Gates.Comb.width s <> 1 then raise Sat_signal_width_not_1
  else of_expr ~solver ~verbose (List.hd s)

open Printf

let report_soln x = 
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
                | Expr.Not(_,(Expr.Var(_) as x)) -> neg_lit (M.find x var_map) :: l
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
