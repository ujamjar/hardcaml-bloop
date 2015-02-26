(* interface to minisat *)

val run : Gates.Consistency.t -> 
  [ `sat of (Expr.t * [ `t | `f | `u ]) list
  | `unsat ]

