(* interface to ocaml-bdd library *)

open Expr

type var_map = int M.t
type t = Bdd.t

(* map of variables to indices - no specific ordering, starts from 1 *)
let var_map var_set = 
  fst @@ S.fold (fun v (m,i) -> M.add v i m, i+1) var_set (M.empty,1) 

let var_set = List.fold_left (fun set o -> S.union set (Expr.find_vars o))

let vars_of_expr e = var_map (find_vars e)
let vars_of_signal s = var_map (var_set S.empty s)
let vars_of_signals s = var_map (List.fold_left var_set S.empty s)

(* recursively construct the bdd XXX use set to control recursion *)
let rec build vars = function
  | T -> Bdd.one
  | F -> Bdd.zero
  | Var _ as x -> M.find x vars
  | Not(_,a) -> Bdd.mk_not (build vars a)
  | Or(_,a,b) -> Bdd.mk_or (build vars a) (build vars b)
  | And(_,a,b) -> Bdd.mk_and (build vars a) (build vars b)
  | Xor(_,a,b) -> build vars Gates.Basic_gates_opt.(((~. a) &. b) |. (a &. (~. b)))

(* create bdd from Expr.t *)
let of_expr vars e = 
  let () = Bdd.set_max_var (M.cardinal vars) in
  let vars = M.fold (fun k v m -> M.add k (Bdd.mk_var v) m) vars M.empty in
  build vars e

(* create bdd from Gates.Comb.t *)
let of_signal vars s = 
  let () = Bdd.set_max_var (M.cardinal vars) in
  let vars = M.fold (fun k v m -> M.add k (Bdd.mk_var v) m) vars M.empty in
  List.map (build vars) s

(* create bdd from list of circuit outputs *)
let of_signals vars s = 
  let () = Bdd.set_max_var (M.cardinal vars) in
  let vars = M.fold (fun k v m -> M.add k (Bdd.mk_var v) m) vars M.empty in
  List.map (List.map (build vars)) s

type soln = (int * bool) list

let is_sat = Bdd.is_sat
let tautology = Bdd.tautology
let num_solutions = Bdd.count_sat
let get_solution = Bdd.any_sat
let all_solutions = Bdd.all_sat

let vars_of_solution var_map soln = 
  let module I = Map.Make(struct type t = int let compare = compare end) in
  let map = M.fold (fun k v m -> I.add v k m) var_map I.empty in
  List.map (fun (v,b) -> if not b then Expr.Not(Expr.id(),I.find v map) else I.find v map) soln

let term_of_solution var_map soln = 
  let vars = vars_of_solution var_map soln in
  List.fold_left (&:) (List.hd vars) (List.tl vars)

(**********************************************************)

(* set of inputs to gates which have been removed *)
module V = Set.Make(struct type t = Expr.t * float let compare (a,_) (b,_) = compare a b end)

type mask_set = S.t

(* circuit depth from Expr.t *)
let rec depth set e = 
  let depth n a = 
    if S.mem a set then 0
    else depth set a
  in
  match e with
  | T | F | Var _ -> 1
  | Not(_,a) -> 1 + depth 0 a
  | And(_,a,b) | Or(_,a,b) | Xor(_,a,b) -> 1 + (max (depth 0 a) (depth 1 b))

(* get output with max depth *)
let find_max_depth set signals = 
  snd @@ List.fold_left (List.fold_left 
    (fun (depth',expr') expr ->
      let depth = depth set expr in 
      if (depth' < 0) || (depth' < depth) then depth, expr
      else depth', expr')) (-1,F) signals 

(* merge input weigths. where both sets contains the same variable
   the weights are added (perhaps a map would be more logical here?) *)
let merge_inputs v0 v1 = 
  (* find inputs shared between sets *)
  let shared = V.inter v0 v1 in
  (* get all the others *)
  let rest = V.diff (V.union v0 v1) shared in
  (* merge shared inputs from both sets *)
  let shared = V.fold 
    (fun var shared ->
      let (var,w0), (_,w1) = V.find var v0, V.find var v1 in
      V.add (var,w0+.w1) shared) 
    shared V.empty
  in
  (* final set *)
  V.union rest shared

(* calculate the weights for all inputs reachable from e *)
let rec max_input_weights (w,s) e = 
  let masked a = S.mem a s in
  match e with
  | T | F -> V.empty
  | Var _ -> V.singleton (e, w)
  | Not(_,a) -> 
    if masked a then V.empty
    else max_input_weights (w,s) a
  | And(_,a,b) | Or(_,a,b) | Xor(_,a,b) -> begin
    match masked a, masked b with
    | true, true -> V.empty
    | false, true -> max_input_weights (w,s) a
    | true, false -> max_input_weights (w,s) b 
    | false, false -> merge_inputs (max_input_weights (w /. 2., s) a) 
                                   (max_input_weights (w /. 2., s) b)
  end

(* select max weight input *)
let max_input_weight v = 
  match V.fold (fun (v,w) best ->
    match best with
    | Some(v',w') when w' > w -> Some(v',w')
    | _ -> Some(v,w)) v None with
  | Some(v,_) -> v
  | None -> failwith "no inputs!"

(* erase input, and connected fanouts, as seen from e *)
let rec erase_input s v e = 
  let erase s x n = 
    if S.mem x s then true, s (* already erased *)
    else (* recurse *)
      erase_input s v x 
  in
  match e with
  | T | F -> true, S.add e s
  | Var _ -> if v=e then true, S.add e s else false, s
  | Not(_,a) -> 
    let erase, s = erase s a 0 in
    if erase then true, S.add e s else false, s
  | And(_,a,b) | Or(_,a,b) | Xor(_,a,b) ->
    let erase0, s = erase s a 0 in
    let erase1, s = erase s b 1 in
    if erase0 && erase1 then true, S.add e s
    else false, s

(* erase input as seen from all outputs *)
let erase_input_from_all s v e = 
  List.fold_left
    (fun s e -> List.fold_left (fun s e -> snd @@ erase_input s v e) s e)
    s e

(* calculate variable order *)
let dynamic_weight_assignment signals = 
  let debug = false in

  (* count reachable inputs *)
  let vars = List.fold_left var_set S.empty signals in
  let n_vars = S.cardinal vars in

  (* iterate until all inputs ordered *)
  let rec iter idx ordered_vars mask_set = 
    let open Printf in
    let () = if debug then printf "**** %i\n" idx in
    (* done when we have found all the variables *)
    if idx > n_vars then ordered_vars
    else
      (* find output with maximum depth *)
      let () = if debug then 
        List.iter (List.iter (fun e -> printf "depth: %i\n" (depth mask_set e))) signals 
      in 
      let out_expr = find_max_depth mask_set signals in
      let () = if debug then printf "selecting output: %s\n" (string_of_t out_expr) in
      (* get weights of all the inputs, and select max *)
      let vars = max_input_weights (1., mask_set) out_expr in
      let _ = if debug then 
        V.iter (fun (v,w) -> printf "weight: %s{%f}\n" (string_of_t v) w) vars 
      in
      let var = max_input_weight vars in
      let _ = if debug then printf "selecting input: %s\n" (string_of_t var) in
      (* erase connections to selected input XXX from all outputs *)
      let mask_set = erase_input_from_all mask_set var signals in
      let _ = if debug then 
        S.iter (fun v -> printf "masked: %s\n" (Expr.string_of_t v)) mask_set 
      in
      (* record input order *)
      let ordered_vars = M.add var idx ordered_vars in
      iter (idx+1) ordered_vars mask_set
  in
  iter 1 M.empty S.empty

