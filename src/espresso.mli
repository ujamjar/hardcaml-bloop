module I : Set.S with type elt = int

module Expand : sig
  val expand : Pcn.Cubelist.t -> Pcn.Cubelist.t
end

module Reduce : sig
  val reduce_cube : Pcn.Cubelist.t -> Pcn.Cube.t -> Pcn.Cube.t
  val reduce : Pcn.Cubelist.t -> Pcn.Cubelist.t
end

module Irredundant : sig
  val irredundant : Pcn.Cubelist.t -> Pcn.Cubelist.t
end

module Cover : sig
  val row_of_list : bool list -> I.t 
  (* list of row sets, to list of row indices *)
  val find : I.t list -> int list
end
