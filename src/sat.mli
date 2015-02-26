(** interface to minisat *)

(** either sat, with an appropriate assignment, or unsat *)
type sat_result = [ `sat of (Expr.t * [ `t | `f | `u ]) list | `unsat ]

(** run solver *)
val eval : ?cleaner:bool -> Gates.Consistency.t -> sat_result

(** run solver on a (1 bit) signal *)
val of_signal : ?cleaner:bool -> Gates.Comb.t -> sat_result

(** print results *)
val report : sat_result -> unit

