(** interface to minisat *)

(** either sat, with an appropriate assignment, or unsat *)
type model = Expr.t * int * [ `t | `f | `u ]
type next_sat_result = unit -> sat_result
and  sat_result = [ `sat of model list * next_sat_result | `unsat ]
type solver = [ 
  | `crypto 
  | `mini
  | `dimacs of [ `crypto | `mini | `pico ]
]

(** run solver on a boolean expression.
 
    Converts the input expression to CNF form using the
    [Tseitin] transformation with optional sharing (default true).

    Returns `unsat if no assignment to the input variables could
    be found that makes the result 1.

    Otherwise returns `sat with a solution for the input variables and
    a function to find the next solution.
 *)
val of_expr : ?solver:solver -> ?verbose:bool -> Expr.t -> sat_result

exception Sat_signal_width_not_1

(** run the solver on a signal.  Raises [Sat_signal_width_not_1] if the signal
    is not 1 bit wide. *)
val of_signal : ?solver:solver -> ?verbose:bool -> Gates.Comb.t -> sat_result

(** print results *)
val report_soln : model list -> unit
val report : sat_result -> unit

(** {2 Depreciated; very slow *)
module Old : sig

  (** run solver *)
  val eval : ?cleaner:bool -> Gates.Consistency.t -> sat_result

  (** run solver on a (1 bit) signal *)
  val of_signal : ?cleaner:bool -> Gates.Comb.t -> sat_result

end

