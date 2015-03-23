(* positional cube notation *)

type v = 
  | T (* variable in true form *) 
  | F (* variable in complement form *)
  | X (* dont care (variable not mentioned in product term *)

let rec filter_none = function
  | [] -> []
  | None :: t -> filter_none t
  | Some(x) :: t -> x :: filter_none t

module Cube = struct

  type t = v array

  let init = Array.init
  let fold = Array.fold_left 
  let map = Array.map 
  let num_vars = Array.length 
  let get = Array.get
  let to_list = Array.to_list
  let of_list = Array.of_list
  let to_array x = x
  let of_array x = x

  exception Bad_cube_value

  let to_bits = function
    | T -> 1
    | F -> 2
    | X -> 3

  let of_bits = function
    | 1 -> T
    | 2 -> F
    | 3 -> X
    | _ -> raise Bad_cube_value

  let intersect c0 c1 = 
    try Some(init (num_vars c0) (fun i -> of_bits @@ (to_bits c0.(i)) land (to_bits c1.(i))))
    with Bad_cube_value -> None

  let supercube c0 c1 = 
    init (num_vars c0) (fun i -> of_bits @@ (to_bits c0.(i)) lor (to_bits c1.(i)))

  let is_dontcare c = fold (fun a v -> a && (v = X)) true c

  let count_dontcare c = fold (fun a v -> if v=X then a+1 else a) 0 c

  let count_true c = fold (fun a v -> if v=T then a+1 else a) 0 c

  let count_compl c = fold (fun a v -> if v=F then a+1 else a) 0 c

  let is_single_var c = count_dontcare c = (num_vars c - 1)

  let single_var_true n i = init n (fun j -> if i=j then T else X)

  let single_var_compl n i = init n (fun j -> if i=j then F else X)

  let one n = init n (fun _ -> X)

  let positive_cofactor i c = 
    match c.(i) with
    | T -> Some(init (num_vars c) (fun j -> if j=i then X else c.(j)))
    | F -> None
    | X -> Some(c)

  let negative_cofactor i c = 
    match c.(i) with
    | F -> Some(init (num_vars c) (fun j -> if j=i then X else c.(j)))
    | T -> None
    | X -> Some(c)

  let print c = 
    let c = map (function T -> "01 " | F -> "10 " | X -> "11 ") c in
    Array.iter print_string c;
    print_string "\n"

end

module Cubelist = struct

  (* different internal implementations as an array of cubes, or set of cubes.
   * The later has the advantage that it automatically merges/removes identical
   * cubes *)
  module type S = sig
    type t 
    val num_cubes : t -> int
    val num_vars : t -> int
    val fold : ('a -> Cube.t -> 'a) -> 'a -> t -> 'a
    val map : (Cube.t -> Cube.t) -> t -> t
    val concat : t list -> t
    val of_list : Cube.t list -> t
    val to_list : t -> Cube.t list 
    val choose : t -> Cube.t * t
    val remove : Cube.t -> t -> t
    val add : Cube.t -> t -> t
    val one : int -> t
    val zero : t
    val print : t -> unit
  end 
  module Arr : S = struct
    type t = Cube.t array
    let num_cubes = Array.length
    let num_vars c = if num_cubes c = 0 then 0 else Cube.num_vars c.(0)
    let fold = Array.fold_left
    let map = Array.map
    let concat = Array.concat
    let of_list = Array.of_list
    let to_list = Array.to_list
    let choose c = c.(0), Array.init (num_cubes c - 1) (fun i -> c.(i+1))
    let remove c cl = of_list @@ List.filter ((<>) c) @@ to_list cl
    let add c cl = Array.concat [ [|c|]; cl ]
    let one n = [| Cube.one n |]
    let zero = [||]
    let print = Array.iter Cube.print
  end
  module Set : S = struct
    module S = Set.Make(struct type t = Cube.t let compare = compare end)
    type t = S.t
    let num_cubes = S.cardinal
    let num_vars c = if num_cubes c = 0 then 0 else Cube.num_vars (S.choose c)
    let fold f a cl = S.fold (fun c a -> f a c) cl a
    let map f cl = S.fold (fun c d -> S.add (f c) d) cl S.empty
    let concat = List.fold_left S.union S.empty
    let of_list l = List.fold_left (fun s x -> S.add x s) S.empty l
    let to_list = S.elements
    let choose c = let x = S.choose c in x, S.remove x c
    let remove = S.remove
    let add = S.add
    let one n = S.singleton (Cube.one n)
    let zero = S.empty
    let print = S.iter Cube.print
  end
  
  include Set

  let fold_var i f = fold (fun a c -> f a (Cube.get c i))

  let map_vars f cl = 
    Array.init (num_vars cl) (fun i -> f i)

  let map_fold_vars f a c = 
    let n_vars = num_vars c in
    Array.init n_vars (fun i -> fold_var i f a c)

  let var_is_only_true i v = 
    fold_var i (fun a c -> a && (c = T || c = X)) true v

  let var_is_only_compl i v = 
    fold_var i (fun a c -> a && (c = F || c = X)) true v

  let var_is_unate i v = var_is_only_true i v || var_is_only_compl i v

  let is_unate cl = 
    let u = map_vars (fun i -> var_is_unate i cl) cl in
    Array.fold_left (&&) true u

  let has_dontcare_cube cl = 
    fold (fun a c -> a || (Cube.is_dontcare c)) false cl

  (* this might need a bit more testing (and possibly a better implementation strategy) *)
  let has_single_var_true_and_compl cl = 
    let cl = of_list @@ List.filter Cube.is_single_var (to_list cl) in
    let x = map_fold_vars 
      (fun a -> function
        | F -> a lor 1
        | T -> a lor 2
        | X -> a) 0 cl
    in
    Array.fold_left (fun a c -> if c=3 then true else a) false x

  let count_true_and_compl cl = 
    map_fold_vars 
      (fun (t,f) -> function T -> (t+1,f) | F -> (t,f+1) | X -> (t,f)) 
      (0,0) cl 

  let cofactor f i cl = 
    to_list cl
    |> List.map (f i)
    |> filter_none
    |> of_list

  let positive_cofactor = cofactor Cube.positive_cofactor
  let negative_cofactor = cofactor Cube.negative_cofactor

  (* convert cubelist to sop form *)
  let sop c = 
    let cube c = 
      List.sort (fun a b -> compare (abs a) (abs b)) @@ fst @@ Cube.fold 
        (fun (a,i) v ->
          match v with 
          | T -> i :: a, i+1
          | F -> -i :: a, i+1
          | X -> a,i+1) ([],1) c 
    in
    fold (fun a c -> cube c :: a) [] c

end

module Tautology = struct

  let not_tautology cl = 
    Cubelist.is_unate cl && not (Cubelist.has_dontcare_cube cl) (* rule 2 *)

  let is_tautology cl = 
    (Cubelist.has_dontcare_cube cl) || (* rule 1 *)
    (Cubelist.has_single_var_true_and_compl cl) (* XXX rule 3 *)

  type sel = 
    | Binate of int * int (* sum and diff *)
    | Unate of int (* num cubes unate in *)

  let selection_metrics : Cubelist.t -> sel array = fun cl ->
    let m = Cubelist.count_true_and_compl cl in
    Array.map 
      (fun (t,f) -> 
        if t=0 then Unate f
        else if f=0 then Unate t 
        else Binate((t+f), abs (t-f))) 
      m

  let best_selection : Cubelist.t -> int = fun cl -> 
    let m = selection_metrics cl in
    let idx,_,_ = 
      Array.fold_left 
        (fun (best,i,m') m -> 
          let better () = (i,i+1,m) in
          match m',m with
          (* better unate *)
          | Unate x', Unate x when x > x' -> better ()
          (* binate always better than unate *)
          | Unate _, Binate(s,d) -> better ()
          (* better binate - most binate *)
          | Binate (s',d'), Binate(s,d) when s > s' -> better ()
          (* better binate - L/R balance *)
          | Binate (s',d'), Binate(s,d) when (s=s') && (d' < d) -> better ()
          (* keep last result *)
          | _ -> (best,i+1,m'))
        (0,0,Unate 0) m
    in
    idx

  let rec check (cl : Cubelist.t) = 
    if is_tautology cl then true
    else if not_tautology cl then false
    else
      let i = best_selection cl in 
      if i=(-1) then failwith "Tautology.check: nothing to select on; can't complete"
      else 
        check (Cubelist.positive_cofactor i cl) && 
        check (Cubelist.negative_cofactor i cl)

end

module Complement = struct

  let simple_complement c =
    let num_vars = Cube.num_vars c in
    Cube.to_list c 
    |> List.mapi 
      (fun i -> function
        | F -> Some(Cube.single_var_true num_vars i)
        | T -> Some(Cube.single_var_compl num_vars i)
        | X -> None) 
    |> filter_none
    |> Cubelist.of_list

  let complement f = 
    let num_vars = Cubelist.num_vars f in

    (* basic boolean operations *)
    let or_ a b = Cubelist.concat [a;b] in
    let and_ i f = 
      Cubelist.map (fun c -> Cube.init num_vars (fun j -> if i=j then T else Cube.get c j)) f
    in
    let nand_ i f = 
      Cubelist.map (fun c -> Cube.init num_vars (fun j -> if i=j then F else Cube.get c j)) f
    in

    (* URP *)
    let rec complement f = 
      if Cubelist.num_cubes f = 0 then Cubelist.one num_vars
      else if Cubelist.has_dontcare_cube f then Cubelist.zero
      else if Cubelist.num_cubes f = 1 then simple_complement (List.hd (Cubelist.to_list f))
      else
        let i = Tautology.best_selection f in
        let p = complement (Cubelist.positive_cofactor i f) in
        let n = complement (Cubelist.negative_cofactor i f) in
        let r = or_ (and_ i p) (nand_ i n) in
        r
    in
    complement f

end

module Calculator = struct

  let not_ a = Complement.complement a
  let or_ a b = Cubelist.concat [a; b]
  let and_ n m = not_ (or_ (not_ n) (not_ m))

  let rec eval (state, cmds) = 
    let rec eval = function
      | `bor(k, n, m) :: t -> (state.(k) <- or_ state.(n) state.(m); eval t)
      | `band(k, n, m) :: t -> (state.(k) <- and_ state.(n) state.(m); eval t)
      | `not(k, n) :: t -> (state.(k) <- not_ state.(n); eval t)
      | `output(k) :: [] -> state.(k)
      | _ -> failwith "ooops, didn't finish with an output command..."
    in
    eval cmds

end

open Expr

let (~:) = Complement.complement
let (&:) = Calculator.and_
let (|:) = Calculator.or_
let (^:) a b = ((~: a &: b) |: (a &: ~: b))
let tautology = Tautology.check

let build e = 
  (* map of variables to indices - no specific ordering, starts from 0 *)
  let var_map var_set = fst @@ S.fold (fun v (m,i) -> M.add v i m, i+1) var_set (M.empty,0) in
  let vars = var_map @@ Expr.find_vars e in
  let n_vars = M.cardinal vars in
  let rec build = function
    | T -> Cubelist.zero
    | F -> Cubelist.one n_vars
    | Var _ as x -> Cubelist.of_list [Cube.single_var_true n_vars (M.find x vars)]
    | Not(Var _ as x) -> Cubelist.of_list [Cube.single_var_compl n_vars (M.find x vars)]
    | Not(a) -> Calculator.not_ (build a)
    | And(a,b) -> Calculator.and_ (build a) (build b)
    | Or(a,b) -> Calculator.or_ (build a) (build b)
    | Xor(a,b) -> build Expr.((~: a &: b) |: (a &: ~: b))
  in
  build e, vars

