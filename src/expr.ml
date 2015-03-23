(* simple boolean expression type *)

type t = 
  | T 
  | F 
  | Var of string * int
  | Not of t 
  | Or of t * t 
  | Xor of t * t 
  | And of t * t

let t = T
let f = F
let var ?(i=0) x = Var(x, i)
let (~:) x = Not x
let (|:) a b = Or(a, b)
let (^:) a b = Xor(a, b)
let (&:) a b = And(a, b)

type t' = t (* hack *)
module X = struct
  type t = t'
  let compare = compare
end
module S = Set.Make(X)
module M = Map.Make(X)

let string_of_t' var_ not_ xor_ b = 
  let rec f level b = 
    let bracket l s = if l > level then "(" ^ s ^ ")" else s in
    match b with
    | T -> "1"
    | F -> "0"
    | Var (s,i) -> var_ s i
    | Not x -> not_ (f level x)
    | And(a, b) -> bracket 0 (f 0 a ^ f 0 b)
    | Or(a, b) -> bracket 1 (f 1 a ^ "+" ^ f 1 b)
    | Xor(a, b) -> bracket 2 (xor_ (f 2 a) (f 2 b))
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
  | Var(s,i) -> s ^ string_of_int i
  | Not s -> "(!" ^ kbdd_string_of_t s ^ ")"
  | And(a, b) -> "(" ^ kbdd_string_of_t a ^ "&" ^ kbdd_string_of_t b ^ ")"
  | Or(a, b) -> "(" ^ kbdd_string_of_t a ^ "+" ^ kbdd_string_of_t b ^ ")"
  | Xor(a, b) -> "(" ^ kbdd_string_of_t a ^ "^" ^ kbdd_string_of_t b ^ ")"

let cofactor x v b = 
  let n = match x with Var(n,i) -> (n,i) | _ -> failwith "must take cofactor wrt a variable" in
  let v = match v with T -> T | F -> F | _ -> failwith "neither +ve nor -ve cofactor" in
  let rec f b = 
    match b with
    | Var (x,j) when n = (x,j) -> v
    | Var (x,j) -> Var(x,j)
    | T -> T
    | F -> F
    | And(a,b) -> And(f a, f b)
    | Or(a,b) -> Or(f a, f b)
    | Xor(a,b) -> Xor(f a, f b)
    | Not(a) -> Not(f a)
  in
  f b

let simplify b = 
  let const a = match a with T -> Some(true) | F -> Some(false) | _ -> None in
  let of_bool a = match a with true -> T | false -> F in
  let not = function
    | Not(a) -> a
    | _ as x -> Not x
  in
  let rec f b = 
    match b with
    | Var _ 
    | T 
    | F -> b
    | And(a,b) -> begin 
      let a, b = f a, f b in
      match const a, const b with
      | Some(a), Some(b) -> of_bool (a && b)
      | Some(false), None | None, Some(false) -> F
      | Some(true), None -> b
      | None, Some(true) -> a
      | None, None -> if a=b then a else And(a,b)
    end
    | Or(a,b) -> begin
      let a, b = f a, f b in
      match const a, const b with
      | Some(a), Some(b) -> of_bool (a || b)
      | Some(false), None -> b
      | None, Some(false) -> a
      | Some(true), None | None, Some(true) -> T
      | None, None -> if a=b then a else Or(a,b)
    end
    | Xor(a,b) -> begin
      let a, b = f a, f b in
      match const a, const b with
      | Some(a), Some(b) -> of_bool (a <> b)
      | Some(false), None -> b
      | None, Some(false) -> a
      | Some(true), None -> not b
      | None, Some(true) -> not a
      | None, None -> if a=b then F else Xor(a,b)
    end
    | Not(a) -> begin
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
  | Not(a) -> find_vars a
  | And(a,b) | Or(a,b) | Xor(a,b) -> S.union (find_vars a) (find_vars b)

type truth_table = (string * int) array * int array

let truth_table f = 
  let module S = Set.Make(struct
    type t = string * int
    let compare = compare
  end) in
  let rec vars = 
    function T -> S.empty | F -> S.empty | Var (x,i) -> S.singleton (x,i) | Not x -> vars x 
           | And(a,b) | Xor(a,b) | Or(a,b) -> S.union (vars a) (vars b)
  in
  let rec eval env = 
    function T -> 1
           | F -> 0
           | Var(x,i) -> (env (x,i))
           | Not(a) -> if eval env a = 0 then 1 else 0
           | And(a,b) -> (eval env a) land (eval env b)
           | Xor(a,b) -> (eval env a) lxor (eval env b)
           | Or(a,b) -> (eval env a) lor (eval env b)
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


