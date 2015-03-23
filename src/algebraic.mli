type t 
exception Only_vars_allowed_in_sop of string

(* to/from internal representation as sets *)
val cubes_of_sop : Expr.t list list -> t
val sop_of_cubes : t -> Expr.t list list

(* algebraic division *)
val divide : t -> t -> t * t

val shared_cubes : t -> Expr.S.t
val is_cube_free : t -> bool

type cokernel = Expr.S.t
type kernel = t

val cokernels_of_var : Expr.t -> t -> t
val find_kernel_of_var : Expr.t -> t -> (cokernel * kernel) option

(* note; list of cokernels/kernels is not unique and doesn't 
   include the function itself *)
val find_kernels : t -> (cokernel * kernel) list

