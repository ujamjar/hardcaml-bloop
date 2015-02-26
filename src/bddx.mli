(* interface to ocaml-bdd library *)

val of_expr : Expr.t -> Bdd.t

val of_signal : Gates.Comb.t -> Bdd.t list

val of_signals : Gates.Comb.t list -> Bdd.t list list

(* dynamic weight assignment.
  
  Given a circuit structure this calculates a variable order.

  It tries to put variables which are furthest from the outputs and that
  also affect the most interface nodes first.

  Implementation note; we maintain a so called 'mask_set' which is 
  a set of nodes that are not longer reachable.  This avoids trying to
  rewrite the circuit graph itself (which is difficult given the 
  implementaton of Expr.t).

 *)

module S : Set.S with type elt = Expr.t 
module V : Set.S with type elt = Expr.t * float

type mask_set = S.t

val depth : mask_set -> Expr.t -> int
val find_max_depth : mask_set -> Gates.Comb.t list -> Expr.t
val merge_inputs : V.t -> V.t -> V.t
val max_input_weights : (float * mask_set) -> Expr.t -> V.t
val max_input_weight : V.t ->  Expr.t
val erase_input : mask_set -> Expr.t -> Expr.t -> bool * mask_set
val erase_input_from_all : mask_set -> Expr.t -> Gates.Comb.t list -> mask_set

val dynamic_weight_assignment : Gates.Comb.t list -> (Expr.t * int) list

