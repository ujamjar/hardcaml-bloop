(* simple boolean expression type *)

type uid = int

type t = 
  | T 
  | F 
  | Var of uid * string * int
  | Not of uid * t 
  | Or of uid * t * t 
  | Xor of uid * t * t 
  | And of uid * t * t

let uid = function
  | F -> 0
  | T -> 1
  | Var(uid,_,_) 
  | Not(uid,_)
  | Or(uid,_,_) 
  | Xor(uid,_,_) 
  | And(uid,_,_) -> uid

let compare_uid (a:int) (b:int) = compare a b

module Uset = Set.Make(struct
  type t = uid
  let compare = compare_uid
end)
module Umap = Map.Make(struct
  type t = uid
  let compare = compare_uid
end)

let id =
  let id = ref 1 in (* 0+1 for constants *)
  (fun () -> incr id; !id)

let t = T
let f = F
let var ?(i=0) x = Var(id(), x, i)
let (~:) x = Not(id(), x)
let (|:) a b = Or(id(), a, b)
let (^:) a b = Xor(id(), a, b)
let (&:) a b = And(id(), a, b)

type t' = t (* hack *)
module X = struct
  type t = t'
  let compare = compare
end
module S = Set.Make(X)
module M = Map.Make(X)

type counts = 
  {
    consts : int;
    vars : int;
    nots : int;
    ors : int;
    xors : int;
    ands : int;
    lookups : int;
  }
(*
let counts e = 
  let consts = ref 0 in
  let vars = ref 0 in
  let nots = ref 0 in
  let ors = ref 0 in
  let xors = ref 0 in
  let ands = ref 0 in
  let rec f = function
    | T | F -> incr consts
    | Var _ -> incr vars
    | Not(a) -> incr nots; f a
    | Or(a,b) -> incr ors; f a; f b
    | Xor(a,b) -> incr xors; f a; f b
    | And(a,b) -> incr ands; f a; f b
  in 
  f e;
  {
    consts = !consts;
    vars = !vars;
    nots = !nots;
    ors = !ors;
    xors = !xors;
    ands = !ands;
    lookups = 0;
  }
*)
let counts set e = 
  let consts = ref 0 in
  let vars = ref 0 in
  let nots = ref 0 in
  let ors = ref 0 in
  let xors = ref 0 in
  let ands = ref 0 in
  let lookups = ref 0 in
  let rec f set e  =
    incr lookups;
    if Uset.mem (uid e) set then set
    else
      let set = Uset.add (uid e) set in
      match e with
      | T | F -> incr consts; set
      | Var _ -> incr vars; set
      | Not(_,a) -> incr nots; f set a
      | Or(_,a,b) -> incr ors; f (f set a) b
      | Xor(_,a,b) -> incr xors; f (f set a) b
      | And(_,a,b) -> incr ands; f (f set a) b
  in
  let set = f set e in
  set, {
    consts = !consts;
    vars = !vars;
    nots = !nots;
    ors = !ors;
    xors = !xors;
    ands = !ands;
    lookups = !lookups;
  }

let zero_counts = 
  {
    consts = 0;
    vars = 0;
    nots = 0;
    ors = 0;
    xors = 0;
    ands = 0;
    lookups = 0;
  }

let add_counts a b = 
  {
    consts = a.consts + b.consts;
    vars = a.vars + b.vars;
    nots = a.nots + b.nots;
    ors = a.ors + b.ors;
    xors = a.xors + b.xors;
    ands = a.ands + b.ands;
    lookups = a.lookups + b.lookups;
  }

let print_counts chan a = 
  Printf.fprintf chan "\
consts = %i
vars = %i
nots = %i
ors = %i
xors = %i
ands = %i
lookups = %i
" 
  a.consts a.vars a.nots a.ors a.xors a.ands a.lookups

let string_of_t' var_ not_ xor_ b = 
  let rec f level b = 
    let bracket l s = if l > level then "(" ^ s ^ ")" else s in
    match b with
    | T -> "1"
    | F -> "0"
    | Var (_,s,i) -> var_ s i
    | Not (_, x) -> not_ (f level x)
    | And(_, a, b) -> bracket 0 (f 0 a ^ f 0 b)
    | Or(_, a, b) -> bracket 1 (f 1 a ^ "+" ^ f 1 b)
    | Xor(_, a, b) -> bracket 2 (xor_ (f 2 a) (f 2 b))
  in
  f 10 b

let string_of_t = 
  string_of_t'
    (fun s i -> s ^ string_of_int i)
    (fun s -> "(" ^ s ^ ")'")
    (fun a b -> a ^ "^" ^ b)

let mathjax_of_t = 
  string_of_t'
    (fun s i -> s ^ "_{" ^ string_of_int i ^ "}")
    (fun s -> "\\overline{" ^ s ^ "}")
    (fun a b -> a ^ " \\oplus " ^ b) 

let rec kbdd_string_of_t = function
  | T -> "1"
  | F -> "0"
  | Var(_, s,i) -> s ^ string_of_int i
  | Not (_,s) -> "(!" ^ kbdd_string_of_t s ^ ")"
  | And(_,a, b) -> "(" ^ kbdd_string_of_t a ^ "&" ^ kbdd_string_of_t b ^ ")"
  | Or(_,a, b) -> "(" ^ kbdd_string_of_t a ^ "+" ^ kbdd_string_of_t b ^ ")"
  | Xor(_,a, b) -> "(" ^ kbdd_string_of_t a ^ "^" ^ kbdd_string_of_t b ^ ")"

let cofactor x v b = 
  let n = match x with Var(_,n,i) -> (n,i) | _ -> failwith "must take cofactor wrt a variable" in
  let v = match v with T -> T | F -> F | _ -> failwith "neither +ve nor -ve cofactor" in
  let rec f b = 
    match b with
    | Var (_,x,j) when n = (x,j) -> v
    | Var (_,x,j) -> Var(id(), x, j)
    | T -> T
    | F -> F
    | And(_,a,b) -> And(id(), f a, f b)
    | Or(_,a,b) -> Or(id(), f a, f b)
    | Xor(_,a,b) -> Xor(id(), f a, f b)
    | Not(_,a) -> Not(id(), f a)
  in
  f b

let simplify b = 
  let const a = match a with T -> Some(true) | F -> Some(false) | _ -> None in
  let of_bool a = match a with true -> T | false -> F in
  let not = function
    | Not(_,a) -> a
    | _ as x -> Not(id(),x)
  in
  let rec f b = 
    match b with
    | Var _ 
    | T 
    | F -> b
    | And(_,a,b) -> begin 
      let a, b = f a, f b in
      match const a, const b with
      | Some(a), Some(b) -> of_bool (a && b)
      | Some(false), None | None, Some(false) -> F
      | Some(true), None -> b
      | None, Some(true) -> a
      | None, None -> if a=b then a else And(id(),a,b)
    end
    | Or(_,a,b) -> begin
      let a, b = f a, f b in
      match const a, const b with
      | Some(a), Some(b) -> of_bool (a || b)
      | Some(false), None -> b
      | None, Some(false) -> a
      | Some(true), None | None, Some(true) -> T
      | None, None -> if a=b then a else Or(id(),a,b)
    end
    | Xor(_,a,b) -> begin
      let a, b = f a, f b in
      match const a, const b with
      | Some(a), Some(b) -> of_bool (a <> b)
      | Some(false), None -> b
      | None, Some(false) -> a
      | Some(true), None -> not b
      | None, Some(true) -> not a
      | None, None -> if a=b then F else Xor(id(),a,b)
    end
    | Not(_,a) -> begin
      let a = f a in
      match const a with
      | Some(true) -> F
      | Some(false) -> T
      | None -> not a
    end
  in
  f b

let shannon_expansion x f =
  let p = cofactor x T f in
  let n = cofactor x F f in
  (x &: p) |: ((~: x) &: n)

let difference v f = (cofactor v T f) ^: (cofactor v F f)
let forall v f = (cofactor v T f) &: (cofactor v F f)
let exists v f = (cofactor v T f) |: (cofactor v F f)

let rec eval xs f = 
  match xs with
  | [] -> f 
  | (x,v)::t -> eval t (cofactor x v f)

(* search for input variables, return as a set *)
let rec find_vars = function
  | T -> S.empty
  | F -> S.empty
  | Var _ as x -> S.singleton x
  | Not(_,a) -> find_vars a
  | And(_,a,b) | Or(_,a,b) | Xor(_,a,b) -> S.union (find_vars a) (find_vars b)

type truth_table = (string * int) array * int array

let truth_table f = 
  let module S = Set.Make(struct
    type t = string * int
    let compare = compare
  end) in
  let rec vars = 
    function T -> S.empty | F -> S.empty 
           | Var (_,x,i) -> S.singleton (x,i) | Not (_,x) -> vars x 
           | And(_,a,b) | Xor(_,a,b) | Or(_,a,b) -> S.union (vars a) (vars b)
  in
  let rec eval env = 
    function T -> 1
           | F -> 0
           | Var(_,x,i) -> (env (x,i))
           | Not(_,a) -> if eval env a = 0 then 1 else 0
           | And(_,a,b) -> (eval env a) land (eval env b)
           | Xor(_,a,b) -> (eval env a) lxor (eval env b)
           | Or(_,a,b) -> (eval env a) lor (eval env b)
  in
  let vars = S.elements (vars f) in
  let varsi = List.mapi (fun i x -> x,i) vars in
  let n_vars = List.length vars in
  Array.of_list vars, Array.init (1 lsl n_vars) 
    (fun i -> eval (fun s -> let j = List.assoc s varsi in (i lsr j) land 1) f)

let html_of_truth_table (headings, data) =
  let h, d = Array.to_list headings, Array.to_list data in
  let n_vars = List.length h in
  let headings = 
      let hd (x,i) = "<th>" ^ (x^string_of_int i) ^ "</th>\n" in
      let hd = String.concat "" ("<th>result</th>" :: (List.map hd h)) in
      String.concat "" ["<tr>\n"; hd; "</tr>\n"]
  in
  let data = List.mapi 
      (fun i res ->
          let dt x = "<td><center>" ^ string_of_int x ^ "</center></td>\n" in
          let dv = Array.init n_vars (fun j -> dt ((i lsr j) land 1)) |> Array.to_list in
          let d = String.concat "" ([ dt res ] @ dv) in
          String.concat "" ["<tr>\n"; d; "</tr>\n"]
      ) d
  in
  (* heading row *)
  "<table>\n" ^ String.concat "" (headings :: data) ^ "</table>\n"

let html_of_truth_tables x = 
  ("<table><tr>" ^ 
    (String.concat "" 
      (List.map (fun x -> "<td>"^(html_of_truth_table x) ^ "</td>") x)) ^ 
  "</tr><table>")


