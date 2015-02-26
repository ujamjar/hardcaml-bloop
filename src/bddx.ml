(* interface to ocaml-bdd library *)

module S = Set.Make(struct type t = Expr.t let compare = compare end)
module M = Map.Make(struct type t = Expr.t let compare = compare end)

open Expr

let rec find_vars = function
  | T -> S.empty
  | F -> S.empty
  | Var _ as x -> S.singleton x
  | Not(a) -> find_vars a
  | And(a,b) | Or(a,b) | Xor(a,b) -> S.union (find_vars a) (find_vars b)

let rec build vars = function
  | T -> Bdd.one
  | F -> Bdd.zero
  | Var _ as x -> M.find x vars
  | Not(a) -> Bdd.mk_not (build vars a)
  | Or(a,b) -> Bdd.mk_or (build vars a) (build vars b)
  | And(a,b) -> Bdd.mk_and (build vars a) (build vars b)
  | Xor(a,b) -> build vars Gates.(((~. a) &. b) |. (a &. (~. b)))

let var_map var_set = 
  fst @@ S.fold (fun v (m,i) -> M.add v (Bdd.mk_var i) m, i+1) var_set (M.empty,1) 

let of_expr e = 
  (* find and create variables *)
  let vars = find_vars e in
  let () = Bdd.set_max_var (S.cardinal vars) in
  let vars = var_map vars in
  (* build bdd *)
  build vars e

let var_set = List.fold_left (fun set o -> S.union set (find_vars o))

let of_signal s = 
  let vars = var_set S.empty s in
  let () = Bdd.set_max_var (S.cardinal vars) in
  let vars = var_map vars in
  (* build bdd *)
  List.map (build vars) s

let of_signals s = 
  let vars = List.fold_left var_set S.empty s  in
  let () = Bdd.set_max_var (S.cardinal vars) in
  let vars = var_map vars in
  (* build bdd *)
  List.map (List.map (build vars)) s

(**********************************************************)

(* set of inputs to gates which have been removed *)
module D = Set.Make(struct type t = Expr.t * int let compare = compare end)
module V = Set.Make(struct type t = Expr.t * float let compare (a,_) (b,_) = compare a b end)

let rec depth set e = 
  let depth n a = 
    if D.mem (e,n) set then 0
    else depth set a
  in
  match e with
  | T | F | Var _ -> 1
  | Not a -> 1 + depth 0 a
  | And(a,b) | Or(a,b) | Xor(a,b) -> 1 + (max (depth 0 a) (depth 1 b))

let find_max_depth set signals = 
  snd @@ List.fold_left (List.fold_left 
    (fun (depth',expr') expr ->
      let depth = depth set expr in 
      if (depth' < 0) || (depth' < depth) then depth, expr
      else depth', expr')) (-1,F) signals 

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

let rec max_input_weights (w,s) e = 
  let masked n = D.mem (e,n) s in
  match e with
  | T | F -> V.empty
  | Var _ -> V.singleton (e, w)
  | Not a -> 
    if masked 0 then V.empty
    else max_input_weights (w,s) a
  | And(a,b) | Or(a,b) | Xor(a,b) -> begin
      match masked 0, masked 1 with
      | true, true -> V.empty
      | false, true -> max_input_weights (w,s) a
      | true, false -> max_input_weights (w,s) b 
      | false, false -> merge_inputs (max_input_weights (w /. 2., s) a) 
                                     (max_input_weights (w /. 2., s) b)
  end

let max_input_weight v = 
  match V.fold (fun (v,w) best ->
    match best with
    | Some(v',w') when w' > w -> Some(v',w')
    | _ -> Some(v,w)) v None with
  | Some(v,_) -> v
  | None -> failwith "no inputs!"

let rec erase_input s v e = 
  let erase s x n = 
    if D.mem (e,n) s then true, s (* already erased *)
    else (* recurse *)
      let erase, s = erase_input s v x in
      if erase then true, D.add (e,n) s else false, s
  in
  match e with
  | T | F -> false, s
  | Var _ -> if v=e then true, s else false, s
  | Not a -> erase s a 0
  | And(a,b) | Or(a,b) | Xor(a,b) ->
    let erase0, s = erase s a 0 in
    let erase1, s = erase s b 1 in
    erase0 && erase1, s

let dynamic_weight_assignment signals = 
  (* count reachable inputs *)
  let vars = List.fold_left var_set S.empty signals in
  let n_vars = S.cardinal vars in

  (* iterate until all inputs ordered *)
  let rec iter idx ordered_vars mask_set = 
    (* done when we have found all the variables *)
    if M.cardinal ordered_vars = n_vars then ordered_vars
    else
      (* find output with maximum depth *)
      let e = find_max_depth mask_set signals in
      (* get weights of all the inputs, and select max *)
      let vars = max_input_weights (1., mask_set) f in
      let var = max_input_weight vars in
      (* erase connections to selected input *)
      let _, mask_set = erase_input mask_set var e in
      (* record input order *)
      let ordered_vars = M.add var idx ordered_vars in
      iter (idx+1) ordered_vars mask_set
  in
  M.bindings @@ iter 1 M.empty D.empty

