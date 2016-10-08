type uid = int
type mig = Pi of uid * string * int | T | M of uid * edge * edge * edge
and compl = NoCompl | Compl
and edge = mig * compl
and output = Po of edge

module Basic_gates : sig
  val t : edge
  val f : edge
  val (&.) : edge -> edge -> edge
  val (|.) : edge -> edge -> edge
  val (~.) : edge -> edge
  val (^.) : edge -> edge -> edge
end

module Base(B : module type of Basic_gates) : HardCaml.Comb.T

