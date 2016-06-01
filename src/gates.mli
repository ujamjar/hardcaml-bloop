(** {2 Combinatorial APi} *)

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

(** {2 Tseitin cnf conversion} *)

(** Uses the Tseitin transformation to convert an 
    arbitrary gate expression to conjunctive normal
    form.

    The optional sharing parameter (default true) uses
    a hash table to detect internally shared nodes and
    can dramatically reduce memory usage and improve
    performance even with the the extra hashing and 
    structural comparison.
*)
module Tseitin : sig

  type cnf_term = int list list
  type cnf_terms = 
    | Var of int
    | Ref of int
    | Term of cnf_term * cnf_terms list
  type t = 
    {
      nterms : int;
      terms : cnf_terms;
      vars : (int, Expr.t) Hashtbl.t;
    }

  (* Tseitin gate consistency functions *)
  val btrue : int -> int list list
  val bfalse : int -> int list list
  val bnot : int -> int -> int list list
  val bnor : int -> int list -> int list list
  val bor : int -> int list -> int list list
  val bnand : int -> int list -> int list list
  val band : int -> int list -> int list list
  val bxor : int -> int -> int -> int list list

  (** Convert an expression to cnf *)
  val of_expr : ?sharing:bool -> Expr.t -> t

  (** Convert a signal to cnf.  Each bit is a seperate cnf expression *)
  val of_signal : ?sharing:bool -> Comb.t -> t list 

end

(** {2 Depreciated Tseitin cnf conversion.  Very slow} *)

(** *OLD AND SLOW* use the Tseitin module instead
 
    Convert boolean expressionn to product-of-sums form using
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

  val btrue : Expr.t -> Expr.t list list
  val bfalse : Expr.t -> Expr.t list list
  (*val bwire : Expr.t -> Expr.t -> Expr.t list list*)
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

