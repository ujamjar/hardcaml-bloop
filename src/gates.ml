open HardCaml

module type Basic_gates = sig
  open Expr
  val (&.) : t -> t -> t
  val (|.) : t -> t -> t
  val (~.) : t -> t
  val (^.) : t -> t -> t
end

module Basic_gates = struct
  (* basic bit operators (no optimisation) *) 
  open Expr

  let (&.) a b = And(id(),a,b)

  let (|.) a b = Or(id(),a,b)

  let (~.) a = Not(id(),a)

  let (^.) a b = Xor(id(),a,b)

end

module Basic_gates_simple_opt = struct
  (* basic bit operators that optimise based on their arguments *)
  open Expr

  let (&.) a b = 
    let open Expr in
    match a, b with
    | _, T -> a
    | T, _ -> b
    | _ -> And(id(),a,b)

  let (|.) a b = 
    let open Expr in
    match a, b with
    | _, F -> a
    | F, _ -> b
    | _ -> Or(id(),a,b)

  let (~.) a = 
    let open Expr in
    match a with
    | F -> T
    | T -> F
    | Not(_,a) -> a 
    | _ -> Not(id(),a)

  let (^.) a b = 
    let open Expr in
    match a, b with
    | _, F -> a
    | F, _ -> b
    | _, T -> ~. a
    | T, _ -> ~. b
    | _ -> Xor(id(),a,b)

end

module Basic_gates_opt = struct
  (* basic bit operators that optimise based on their arguments *)
  open Expr

  let (&.) a b = 
    let open Expr in
    match a, b with
    | _, F 
    | F, _ -> F
    | _, T -> a
    | T, _ -> b
    (*| a, b when a=b -> a
    | Not(a),b when a=b -> F
    | a,Not(b) when a=b -> F*)
    | _ -> And(id(),a,b)

  let (|.) a b = 
    let open Expr in
    match a, b with
    | _, T 
    | T, _ -> T
    | _, F -> a
    | F, _ -> b
    (*| a, b when a=b -> a
    | Not(a),b when a=b -> T
    | a,Not(b) when a=b -> T*)
    | _ -> Or(id(),a,b)

  let (~.) a = 
    let open Expr in
    match a with
    | F -> T
    | T -> F
    | Not(_,a) -> a 
    | _ -> Not(id(),a)

  let (^.) a b = 
    let open Expr in
    match a, b with
    | _, F -> a
    | F, _ -> b
    | _, T -> ~. a
    | T, _ -> ~. b
    | _ -> Xor(id(),a,b)

end

module Basic_gates_derive_xor(Basic_gates : Basic_gates) = struct
  include Basic_gates
  let (^.) a b = ((~. a) &. b) |. (a &. (~. b))
end

module Base(Basic_gates : Basic_gates) = struct
  open Basic_gates
  open List

  (* combinatorial API implemented using 'simple' gates *)

  type t = Expr.t list

  let empty = []

  let gnd = [Expr.F]
  let vdd = [Expr.T]

  let width = List.length

  let const v = 
    let to_int = function '0' -> Expr.F | '1' -> Expr.T | _ -> failwith "invalid constant" in
    let len = String.length v in
    let rec const b i = 
      if len = i then b 
      else const (to_int v.[i] :: b) (i+1)
    in
    rev (const [] 0)

  let consti l v = const (Utils.bstr_of_int l v) 

  let concat = concat

  let select s h l = 
    let rec sel b i = 
      match b with
      | [] -> []
      | hd::tl -> 
        if i > h then []
        else if i >= l then hd :: sel tl (i+1)
        else sel tl (i+1)
    in
    rev (sel (rev s) 0)

  let to_int _ = failwith "Can't convert simple gates to int"

  let assert_same_width l = 
    let w = width (hd l) in
    let _ = iter (fun b -> assert (w = width b)) l in
    w

  let (~:) a = map (~.) a

  let (|:) a b = 
    let _ = assert_same_width [a; b] in
    map2 (|.) a b
  
  let (&:) a b = 
    let _ = assert_same_width [a; b] in
    map2 (&.) a b
  
  let (^:) a b = 
    let _ = assert_same_width [a; b] in
    map2 (^.) a b

  let reduce f l = fold_left f (hd l) (tl l)

  let (==:) a b = [reduce (&.) (~: (a ^: b))]

  let rec repeat s n = 
    if n = 0 then empty
    else if n = 1 then s
    else concat [s; repeat s (n-1)]

  let mux sel vals =
    let last_elem = hd (rev vals) in
    Array.init (1 lsl (width sel))
      (fun i -> 
        let e = ((consti (width sel) i) ==: sel) in
        let v = try List.nth vals i with _ -> last_elem in
        let e = repeat e (width v) in
        e &: v) |> Array.to_list |> reduce (|:) 

  let (+:) a b = 
    let fa x y z = (x &. y) |. (x &. z) |. (y &. z), x ^. y ^. z in
    (fst (List.fold_left2 (fun (res,carry) a b -> 
      let carry, sum = fa a b carry in 
      sum::res, carry) ([],Expr.F) (rev a) (rev b)))

  let (-:) a b = 
    let fs x y z = ((~. x) &. (y |. z)) |. (x &. y &. z), (x ^. y) ^. z in
    (fst (List.fold_left2 (fun (res,carry) a b -> 
      let carry, sum = fs a b carry in 
      sum::res, carry) ([],Expr.F) (rev a) (rev b)))
  
  let zero = repeat gnd 
  let msb x = select x (width x - 1) (width x - 1)
  let bits = List.map (fun x -> [x])

  let ( *: ) a b = 
    let _,r = List.fold_left (fun (i,acc) b -> 
      let acc = concat [gnd; acc] in
      let a = concat [ gnd ; a ; repeat gnd i ] in
      i+1, (+:) acc ((&:) a (repeat b (width a)))
    ) (0,(zero (width a))) (rev (bits b)) in
    r
  
  let ( *+ ) a b = 
    let last = (width b) - 1 in
    let _,r = List.fold_left (fun (i,acc) b -> 
      let acc = concat [msb acc; acc] in
      let a = concat [ msb a; a ; repeat gnd i ] in
      i+1, (if i = last then (-:) else (+:)) acc ((&:) a (repeat b (width a)))
    ) (0,(zero (width a))) (rev (bits b)) in
    r
    
  let mux2 c a b = (c &. a) |. ((~. c) &. b)

  let (<:) a b =
    let rec less a b = 
      match a,b with
      | [],[] -> Expr.F
      | a::t0,b::t1 -> 
        mux2 (~. a &. b) Expr.T (mux2 (a &. ~. b) Expr.F (less t0 t1))
      | _ -> failwith "args not same width"
    in
    [less a b]

  let to_string s = 
    String.concat ""
      (mapi (fun i s -> "[" ^ string_of_int i ^ "] " ^ Expr.string_of_t s ^ "\n") (rev s))

  let to_bstr _ = failwith "cannot do to_bstr"

  (** we need something to do inputs.  for now, we'll do it over the applied functor *)
  let (<==) a b = ()
  let (--) a b = a
  let wire w = empty 

end

module type Comb = sig
  include HardCaml.Comb.S with type t = Expr.t list
  val forall : t -> t -> t
  val exists : t -> t -> t
  val counts : Expr.Uset.t -> t -> Expr.Uset.t * Expr.counts
end

module Make_comb(Basic_gates : Basic_gates) = struct
  include HardCaml.Comb.Make(Base(Basic_gates))
  let input name bits = Array.init bits 
    (fun i -> Expr.Var(Expr.id(), name, (bits-i-1))) |> Array.to_list

  let forall x f = 
    let forall_f f = List.fold_right Expr.forall x f in
    List.map forall_f f

  let exists x f = 
    let exists_f f = List.fold_right Expr.exists x f in
    List.map exists_f f

  let counts set s = 
    let f (s,t) x =
      let s, t' = Expr.counts set x in
      s, Expr.add_counts t t'
    in
    List.fold_left f (set,Expr.zero_counts) s

end

module Comb = Make_comb(Basic_gates_opt)

module Tseitin = struct

  include Sattools.Tseitin.Make(struct
      type t = int
      let (~:) a = - a
  end)

  module Vmap = Map.Make(struct
    type t = int
    let compare (a:int) (b:int) = compare a b
  end)

  type cnf_term = int list list
  type cnf_terms = 
    | Var of int
    | Ref of int
    | Term of cnf_term * cnf_terms list
  type t = 
    {
      nterms : int;
      terms : cnf_terms;
      top_term : int; (* probably always 1? *)
      vars : Expr.t Vmap.t;
      map : int Expr.Umap.t;
    }

  let rec of_expr map vars new_idx e = 
    match Expr.Umap.find Expr.(uid e) map with
    | _ as id -> map, vars, id, Ref id
    | exception Not_found -> begin
      let id = new_idx () in
      let map = Expr.Umap.add (Expr.uid e) id map in
      let const f = map, vars, id, Term(f id, []) in
      let bnot x = 
        let map, vars, eid, ecnf = of_expr map vars new_idx x in
        map, vars, id, Term( bnot id eid, [ecnf] )
      in
      let var e = 
        map, Vmap.add id e vars, id, Var id
      in
      let binary f a b =
        let map, vars, aid, acnf = of_expr map vars new_idx a in
        let map, vars, bid, bcnf = of_expr map vars new_idx b in
        map, vars, id, Term(f id [aid; bid], [acnf; bcnf])
      in
      let bxor a b =
        let map, vars, aid, acnf = of_expr map vars new_idx a in
        let map, vars, bid, bcnf = of_expr map vars new_idx b in
        map, vars, id, Term(bxor id aid bid, [acnf; bcnf])
      in

      match e with
      | Expr.T -> const btrue 
      | Expr.F -> const bfalse 
      | Expr.Var _ -> var e

      (*| Expr.Not(Expr.And(e0, e1)) -> binary e bnand e0 e1
      | Expr.Not(Expr.Or(e0, e1)) -> binary e bnor e0 e1*)
      | Expr.And(_, e0, e1) -> binary band e0 e1
      | Expr.Or(_, e0, e1) -> binary bor e0 e1
      | Expr.Xor(_, e0, e1) -> bxor e0 e1
      | Expr.Not(_, e0) -> bnot e0
    end

  let of_expr e = 
    let idx = ref 0 in
    let new_idx () = incr idx; !idx in (* 1... *)
    let map,vars = Expr.Umap.empty, Vmap.empty in
    let map, vars, top_term, terms = of_expr map vars new_idx e in
    {
      nterms = !idx;
      terms;
      top_term;
      vars;
      map;
    }

  let of_signal = List.map of_expr 

end

