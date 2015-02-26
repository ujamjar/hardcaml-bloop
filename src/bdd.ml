(* interface to ocaml-bdd library *)

module S = Set.Make(struct type t = Expr.t let compare = compare end)

let of_expr e = 
  let open Expr in
  let rec find_vars = function
    | T -> set
    | F -> set
    | Var _ as x -> S.singleton x
    | Not(a) -> find_vars a
    | And(a,b) | Or(a,b) | Xor(a,b) -> S.union (find_vars a) (find_vars b)
  in
  find_vars e

