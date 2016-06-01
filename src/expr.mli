(** {2 basic boolean expression type} *)

type t = 
  | T 
  | F 
  | Var of string * int
  | Not of t 
  | Or of t * t 
  | Xor of t * t 
  | And of t * t

(** {2 boolean expression constants, variables and operators} *)

val t : t
val f : t
val var : ?i:int -> string -> t
val (~:) : t -> t
val (|:) : t -> t -> t
val (^:) : t -> t -> t
val (&:) : t -> t -> t

(** set of exprs *)
module S : Set.S with type elt = t 

(** map of exprs *)
module M : Map.S with type key = t

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

val counts : t -> counts
val zero_counts : counts
val add_counts : counts -> counts -> counts
val print_counts : out_channel -> counts -> unit

(** convert to string *)
val string_of_t : t -> string

(** convert to mathjax string *)
val mathjax_of_t : t -> string

(** convert to kbdd compatible expression *)
val kbdd_string_of_t : t -> string

(** [cofactor x p f] computes the cofactor of f wrt to x.  [p=t] for
    positive cofactor and [p=f] for negative cofactor *)
val cofactor : t -> t -> t -> t

(** boolean difference *)
val difference : t -> t -> t

(** universal quantification *)
val forall : t -> t -> t

(** existential quantification *)
val exists : t -> t -> t

(** perform basic simplifications *)
val simplify : t -> t

(** [F = xF_x + x'F_x'] *)
val shannon_expansion : t -> t -> t

(** evaluate at each given var by taking given cofactor *)
val eval : (t * t) list -> t -> t

(** find set of vars in expr *)
val find_vars : t -> S.t

type truth_table = (string * int) array * int array

val truth_table : t -> truth_table

val html_of_truth_table : truth_table -> string

val html_of_truth_tables : truth_table list -> string



