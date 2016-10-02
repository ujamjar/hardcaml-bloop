(** interface to minisat *)

(** computed value for some input vector *)
type vec_result = string * Sattools.Lbool.t array

type next_sat_result = unit -> sat_result

(** either sat, with an appropriate assignment, or unsat *)
and  sat_result = (vec_result list * next_sat_result) Sattools.Result.t

(** run solver on a boolean expression.
 
    Converts the input expression to CNF form using the
    [Tseitin] transformation.

    Returns `unsat if no assignment to the input variables could
    be found that makes the result true.

    Otherwise returns `sat with a solution for the input variables and
    a function to find the next solution.

    Specific solutions can be banned by providing an expression which should
    evaluate to [var] or [~: var].  Overly complex expressions (those which
    can't be constant optimised to the required form) will raise an 
    exception.  For convenience, expressions resulting in T or F will be ignored.
 *)
val of_expr : ?solver:string -> ?verbose:bool -> ?banned:Expr.t list list -> 
  Expr.t -> sat_result

exception Sat_signal_width_not_1
exception Sat_banned_expr_too_complex

(** run the solver on a signal.  Raises [Sat_signal_width_not_1] if the signal
    is not 1 bit wide. 

    To ban an input variable from taking a particular value use an expression
    like [~: (a ^:. 23)].
*)
val of_signal : ?solver:string -> ?verbose:bool -> ?banned:Gates.Comb.t list ->
  Gates.Comb.t -> sat_result

(** convert result to a string *)
val string_of_vec_result : ?unknown:char -> vec_result -> string

(** print results *)
val report_soln : vec_result list -> unit
val report : sat_result -> unit

