(** {2 Basic gates} *)

module type Basic_gates = sig
  open Expr
  val (&.) : t -> t -> t
  val (|.) : t -> t -> t
  val (~.) : t -> t
  val (^.) : t -> t -> t
end

(** Simple representation of basic and, or, xor and not gates *)
module Basic_gates : Basic_gates

(** Basic optimisation, but dont allow variables to disappear *)
module Basic_gates_simple_opt : Basic_gates

(** Basic optimisations *)
module Basic_gates_opt : Basic_gates

(** Replace xor basic gate with boolean equation *)
module Basic_gates_derive_xor(Basic_gates : Basic_gates) : Basic_gates

(** {2 Combinatorial APi} *)

(** Combinatorial API implemented over Expr.t *)
module type Comb = sig
  include HardCaml.Comb.S with type t = Expr.t list
  val forall : t -> t -> t
  val exists : t -> t -> t
  val counts : Expr.Uset.t -> t -> Expr.Uset.t * Expr.counts
end

(** Build combinatorial API from [Basic_gates] *)
module Make_comb(Basic_gates : Basic_gates) : Comb

(** Combbinatorial API built with [Basic_gates_opt] *)
module Comb : Comb

(** {2 Tseitin cnf conversion} *)

(** Uses the Tseitin transformation to convert an 
    arbitrary gate expression to conjunctive normal
    form.  *)
module Tseitin : sig

  module Vmap : Map.S with type key = int

  type cnf_term = int list list
  type cnf_terms = 
    | Var of int
    | Ref of int
    | Term of cnf_term * cnf_terms list
  type t = 
    {
      nterms : int;
      terms : cnf_terms;
      top_term : int;
      vars : Expr.t Vmap.t;
      map : int Expr.Umap.t;
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
  val of_expr : Expr.t -> t

  (** Convert a signal to cnf.  Each bit is a seperate cnf expression *)
  val of_signal : Expr.t list -> t list 

end

