(* interface to ocaml-bdd library *)

type var_map = int Expr.M.t
type t = Bdd.t

val vars_of_expr : Expr.t -> var_map

val vars_of_signal : Gates.Comb.t -> var_map

val vars_of_signals : Gates.Comb.t list -> var_map

val of_expr : var_map -> Expr.t -> t

val of_signal : var_map -> Gates.Comb.t -> t list

val of_signals : var_map -> Gates.Comb.t list -> t list list

val is_sat : t -> bool

val tautology : t -> bool

val num_solutions : t -> Int64.t

type soln = (int * bool) list

val get_solution : t -> soln

val all_solutions : t -> soln list

val vars_of_solution : var_map -> soln -> Expr.t list

val term_of_solution : var_map -> soln -> Expr.t 

(* dynamic weight assignment.
  
  Given a circuit structure this calculates a variable order hopefully
  suitable for a BDD.

  It tries to put variables which are furthest from the outputs and that
  also affect the most internal nodes first.

  Implementation note; we maintain a so called 'mask_set' which is 
  a set of nodes that are not longer reachable.  This avoids trying to
  rewrite the circuit graph itself (which is difficult given the 
  implementaton of Expr.t).

 *)

module V : Set.S with type elt = Expr.t * float

type mask_set = Expr.S.t

val depth : mask_set -> Expr.t -> int
val find_max_depth : mask_set -> Gates.Comb.t list -> Expr.t
val merge_inputs : V.t -> V.t -> V.t
val max_input_weights : (float * mask_set) -> Expr.t -> V.t
val max_input_weight : V.t ->  Expr.t
val erase_input : mask_set -> Expr.t -> Expr.t -> bool * mask_set
val erase_input_from_all : mask_set -> Expr.t -> Gates.Comb.t list -> mask_set

val dynamic_weight_assignment : Gates.Comb.t list -> var_map

