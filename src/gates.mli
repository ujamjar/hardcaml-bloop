val (&.) : Expr.t -> Expr.t -> Expr.t 
val (|.) : Expr.t -> Expr.t -> Expr.t 
val (^.) : Expr.t -> Expr.t -> Expr.t 
val (~.) : Expr.t -> Expr.t 

(** combinatorial API implemented over Expr.t *)
module Comb : sig 
  include HardCaml.Comb.S with type t = Expr.t list
  val forall : t -> t -> t
  val exists : t -> t -> t
end

(** Convert boolean expressionn to product-of-sums form using
    consistency functions.  Such expressions have the same
    satisfiability but are not strictly equal.  Unlike the 
    conversions in Sop and Pos the expansion is linear rather
    than exponential *)
module Consistency : sig
  module Svar : (Set.S with type elt = Expr.t)
  module Sexpr : (Set.S with type elt = Svar.t)
  type t = 
    {
      input_vars : Svar.t;
      temp_vars : Svar.t;
      exprs : Sexpr.t;
      out_var : Expr.t;
    }
  type temp_var = unit -> Expr.t
  val temp_var : unit -> Expr.t

  (*val btrue : Expr.t -> Expr.t list list
  val bfalse : Expr.t -> Expr.t list list*)
  val bwire : Expr.t -> Expr.t -> Expr.t list list
  val bnot : Expr.t -> Expr.t -> Expr.t list list
  val bnor : Expr.t -> Expr.t list -> Expr.t list list
  val bor : Expr.t -> Expr.t list -> Expr.t list list
  val bnand : Expr.t -> Expr.t list -> Expr.t list list
  val band : Expr.t -> Expr.t list -> Expr.t list list
  val bxor : Expr.t -> Expr.t -> Expr.t -> Expr.t list list

  val to_gates : Expr.t list list -> Expr.t
  val to_sexpr : Expr.t list list -> Sexpr.t

  val of_expr : Expr.t -> t 
  val of_signal : Comb.t -> t list 

  val to_string : t -> string

  val remove_constants : t -> [ `unsat | `expr of t]

end

