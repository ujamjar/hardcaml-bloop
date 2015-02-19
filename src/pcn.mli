(** Positional cube notation representated as array of arrays *)

type v = 
  | T (* variable in true form *) 
  | F (* variable in complement form *)
  | X (* dont care (variable not mentioned in product term *)

(** operations on a single cube *)
module Cube : sig
  type t = v array
  val fold : ('a -> v -> 'a) -> 'a -> t -> 'a
  val map : (v -> v) -> t -> t
  val num_vars : t -> int

  val is_dontcare : t -> bool
  val count_dontcare : t -> int
  val count_true : t -> int
  val count_compl : t -> int
  val is_single_var : t -> bool
  val single_var_true : int -> int -> t
  val single_var_compl : int -> int -> t

  val one : int -> t

  val positive_cofactor : int -> t -> t option
  val negative_cofactor : int -> t -> t option

  val print : t -> unit
end

(** operations on cube lists *)
module Cubelist : sig
  type t = Cube.t array
  val num_cubes : t -> int
  val num_vars : t -> int
  val fold : ('a -> Cube.t -> 'a) -> 'a -> t -> 'a
  val map : (Cube.t -> Cube.t) -> t -> t
  
  val fold_var : int -> ('a -> v -> 'a) -> 'a -> t -> 'a
  val map_vars : (int -> 'a) -> t -> 'a array
  val map_fold_vars : ('a -> v -> 'a) -> 'a -> t -> 'a array

  val var_is_only_true : int -> t -> bool
  val var_is_only_compl : int -> t -> bool
  val var_is_unate : int -> t -> bool
  val is_unate : t -> bool
  val has_dontcare_cube : t -> bool
  val has_single_var_true_and_compl : t -> bool

  val count_true_and_compl : t -> (int * int) array

  val positive_cofactor : int -> t -> t
  val negative_cofactor : int -> t -> t

  val one : int -> t
  val zero : t

  val print : t -> unit
end

(** compute tautology *)
module Tautology : sig

  val not_tautology : Cubelist.t -> bool
  val is_tautology : Cubelist.t -> bool

  type sel = 
    | Binate of int * int (* sum and diff *)
    | Unate of int (* num cubes unate in *)

  val selection_metrics : Cubelist.t -> sel array
  val best_selection : Cubelist.t -> int

  val check : Cubelist.t -> bool
end

(** compute complement *)
module Complement : sig
  val simple_complement : Cube.t -> Cubelist.t
  val complement : Cubelist.t -> Cubelist.t
end

(** a calculator over cubelists *)
module Calculator : sig

  val not_ : Cubelist.t -> Cubelist.t
  val or_ : Cubelist.t -> Cubelist.t -> Cubelist.t
  val and_ : Cubelist.t -> Cubelist.t -> Cubelist.t

  val eval : Cubelist.t array * 
    [ `not of int * int 
    | `bor of int * int * int 
    | `band of int * int * int 
    | `output of int ] list ->
    Cubelist.t 
end


