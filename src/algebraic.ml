(* algebraic model *)

open Expr

module Scubes = struct
  include Set.Make(struct type t = S.t let compare = S.compare end)
  (* dont add empty cube to sop *)
  let add c s = 
    if c = S.empty then s
    else add c s
end

type t = Scubes.t

exception Only_vars_allowed_in_sop of string

let check_cube = List.iter 
  (function Var _ -> () 
          | Not(Var _) -> () (* allow x', treat as different to x *)
          | _ as x -> raise (Only_vars_allowed_in_sop (string_of_t x)))

let cubes_of_sop f = 
  Scubes.of_list (List.map (fun c -> check_cube c; S.of_list c) f) 

let sop_of_cubes x = List.map S.elements (Scubes.elements x)

(* algebraic division.
 * note; f must be "minimal with respect to single-cube containment"
 * (no one cube completely covered by other cubes in sop cover)
 * may need irredundant operation first! *)
let divide f d = 
  (* calculate the quotient *)
  let q = Scubes.fold 
    (fun d_cube a ->
      let terms = 
        Scubes.fold 
          (fun f_cube a -> 
            if S.subset d_cube f_cube then Scubes.add (S.diff f_cube d_cube) a 
            else a) 
          f Scubes.empty
      in
      match a with
      | None -> Some(terms)
      | Some(terms') -> Some(Scubes.inter terms' terms)) 
    d None 
  in
  (* calculate the remainder *)
  let muls a b = 
    let b' = Scubes.elements b in
    Scubes.fold (fun cube_a a ->
      let t = List.map (S.union cube_a) b' in
      Scubes.union (Scubes.of_list t) a) 
      a Scubes.empty
  in
  match q with
  | None -> Scubes.empty, f
  | Some(q) -> q, Scubes.diff f (muls q d)

let reduce f cs = 
  match Scubes.fold (fun c' c ->
    match c with
    | None -> Some c'
    | Some(c) -> Some(f c c')) cs None with
  | None -> failwith "scube reduce"
  | Some(c) -> c

let shared_cubes cs = reduce S.inter cs
let is_cube_free cs = shared_cubes cs = S.empty

type cokernel = Expr.S.t
type kernel = t

let cokernels_of_var var f = 
  Scubes.fold (fun cube x ->
    if S.mem var cube then Scubes.add cube x
    else x) f Scubes.empty

let find_kernel_of_var var f = 
  let co = cokernels_of_var var f in
  if Scubes.cardinal co > 1 then
    let co = shared_cubes co in
    let q, _ = divide f (Scubes.singleton co) in (* can do much more efficient division here! *)
    Some(co, q)
  else 
    None

(* find kernels of f *)
let rec find_kernels fn = 
  (* list of variables in function *)
  let vars = S.elements @@ Scubes.fold S.union fn S.empty in
  let rec f vars = 
    match vars with 
    | [] -> []
    | var::vars -> begin
      match find_kernel_of_var var fn with
      | None -> f vars (* didn't find anything, try next var *)
      | Some(co,k) ->
        (* co,k is a cokernel and kernel, recurse on the kernel *)
        let r = find_kernels k in
        (* add co into the list of returned cokernels *)
        let r = List.map (fun (c,k) -> S.union c co, k) r in
        List.concat [ [co,k]; r ; f vars ]
    end
  in
  f vars

