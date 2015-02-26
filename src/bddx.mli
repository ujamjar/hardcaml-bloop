(* interface to ocaml-bdd library *)

val of_expr : Expr.t -> Bdd.t

val of_signal : Gates.Comb.t -> Bdd.t list

val of_signals : Gates.Comb.t list -> Bdd.t list list

(* dynamic weight assignment *)

module D : Set.S with type elt = Expr.t * int
module V : Set.S with type elt = Expr.t * float

val depth : D.t -> Expr.t -> int
val find_max_depth : D.t -> Gates.Comb.t list -> Expr.t
val merge_inputs : V.t -> V.t -> V.t
val max_input_weights : (float * D.t) -> Expr.t -> V.t
val max_input_weight : V.t ->  Expr.t
val erase_input : D.t -> Expr.t -> Expr.t -> bool * D.t

val dynamic_weight_assignment : Gates.Comb.t list -> (Expr.t * int) list

