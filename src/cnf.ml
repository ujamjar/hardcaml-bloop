open HardCaml

module Tseitin = Sattools.Tseitin.Make(struct
    type t = int
    let (~:) a = - a
end)

type uid = int
type terms = int list list
type wire_name = (string * int) list ref
type 'a sat = 
  (* tracking sat clauses *)
  | P of uid * terms
  | C of uid * terms * 'a sat list
  (* wires *)
  | W of uid * wire_name * terms ref * 'a sat ref 
  | E

type relabelled
type unlabelled

module Sat = struct

  open Tseitin

  type t = unlabelled sat list

  let uid = 
    let x = ref 0 in
    (fun () -> incr x; !x)

  let get_uid = function 
    | P(uid,_) -> uid 
    | C(uid,_,_) -> uid
    | W(uid,_,_,_) -> uid 
    | E -> failwith "uid of empty signal"

  let gnd = let uid = uid () in P(uid, bfalse uid)
  let vdd = let uid = uid () in P(uid, btrue uid)

  let width s = List.length s

  let const v = 
    let len = String.length v in
    let rec const b i = 
      if len = i then b 
      else 
        let x = match v.[i] with '0' -> gnd | '1' -> vdd | _ -> failwith "const" in
        const (x :: b) (i+1)
    in
    List.rev (const [] 0)

  let empty = []

  let select s h l = 
    let rec sel b i = 
      match b with
      | [] -> []
      | hd::tl -> 
        if i > h then []
        else if i >= l then hd :: sel tl (i+1)
        else sel tl (i+1)
    in
    List.rev (sel (List.rev s) 0)

  let concat l = List.concat l

  let bnot a =
    let uid = uid () in
    C(uid, bnot uid (get_uid a), [a])

  let (~:) = List.map bnot

  let bop2 op a b = 
    let uid = uid () in
    C(uid, op uid [get_uid a; get_uid b], [a;b])

  let (&:) = List.map2 (bop2 band)

  let (|:) = List.map2 (bop2 bor) 

  let bxor a b = 
    let uid = uid () in
    C(uid, bxor uid (get_uid a) (get_uid b), [a;b])

  let (^:) = List.map2 bxor 

  let wire n = 
    Array.to_list @@ Array.init n (fun i -> W(uid(), ref [], ref [], ref E))

  let (<==) a b = 
    assert (width a = width b);
    List.iter2 
      (fun a b ->
        match a with
        | W(uid, names, t, r) -> begin
          t := bwire uid (get_uid b);
          r := b 
        end
        | _ -> failwith "not a wire") a b

  let name_bit s n b = 
    match s with 
    | W(_,names,_,_) -> begin names := (n,b) :: !names; s end
    | _ -> s

  let (--) s n = 
    let w = width s in
    List.mapi (fun i s -> name_bit s n (w-i-1)) s

  let to_bstr _ = failwith "to_bstr"
  let to_int _ = failwith "to_int"
  let to_string _ = failwith "to_string"
end

module Comb : Comb.S with type t = unlabelled sat list = Comb.Make(Transform.MakeCombGates(Sat))

module M = Map.Make(struct type t = int let compare = compare end) 

(*
let relabel s = 
  let mk_uid = let uid = ref 0 in (fun () -> incr uid; !uid) in
  let rec relabel map s = 
    let open Sat in
    let find map x = 
      try if x < 0 then - (M.find (-x) map) else M.find x map
      with _ -> failwith ("map: " ^ string_of_int x) in
    let relabel_terms map uid' terms = 
      let uid, map = 
        if M.mem uid' map then 
          find map uid', map
        else
          let uid = mk_uid () in
          let map = M.add uid' uid map in
          uid, map
      in
      map, uid, List.map (List.map (find map)) terms
    in
    match s with
    | P (uid, terms) -> 
      let map, uid, terms = relabel_terms map uid terms in
      map, P(uid, terms)
    | C (uid, terms, args) -> 
      let map, args = List.fold_left 
          (fun (map,args) x -> 
             let map, x = relabel map x in
             map, x::args) 
          (map,[]) args
      in
      let map, uid, terms = relabel_terms map uid terms in
      map, C(uid, terms, List.rev args)
    | W(uid, names, terms, arg) ->
      let map, arg = relabel map !arg in
      let map, uid, terms = relabel_terms map uid !terms in
      map, W(uid, names, ref terms, ref arg)
    | E -> map, E
  in
  snd @@ relabel M.empty s
*)    

let relabel s = 
  let mk_uid = let uid = ref 0 in (fun () -> incr uid; !uid) in
  let rec relabel vmap map s = 
    let open Sat in
    let find map x = 
      try if x < 0 then - (M.find (-x) map) else M.find x map
      with _ -> failwith ("map: " ^ string_of_int x) in
    let relabel_terms map uid' terms = 
      let uid, map = 
        if M.mem uid' map then 
          find map uid', map
        else
          let uid = mk_uid () in
          let map = M.add uid' uid map in
          uid, map
      in
      map, uid, List.map (List.map (find map)) terms
    in
    let vadd uid vmap map s = M.add uid s vmap, map, s in
    if s <> E && M.mem (Sat.get_uid s) vmap then 
      vmap, map, M.find (Sat.get_uid s) vmap
    else
      match s with
      | P (uid', terms) -> 
        let map, uid, terms = relabel_terms map uid' terms in
        vadd uid' vmap map (P(uid, terms))
      | C (uid', terms, args) -> 
        let vmap, map, args = List.fold_left 
            (fun (vmap, map, args) x -> 
               let vmap, map, x = relabel vmap map x in
               vmap, map, x::args) 
            (vmap, map, []) args
        in
        let map, uid, terms = relabel_terms map uid' terms in
        vadd uid' vmap map (C(uid, terms, List.rev args))
      | W(uid', names, terms, arg) ->
        let vmap, map, arg = relabel vmap map !arg in
        let map, uid, terms = relabel_terms map uid' !terms in
        vadd uid' vmap map (W(uid, names, ref terms, ref arg))
      | E -> vmap, map, E
  in
  let _,_,s = relabel M.empty M.empty s in
  s

let nterms s = 
  let rec count s = 
    let sum_terms t = List.length t in
    let sum_args a = List.fold_left (fun a b -> a + count b) 0 a in
    match s with
    | P (uid, terms) -> sum_terms terms
    | C (uid, terms, args) -> sum_terms terms + sum_args args
    | W (uid, _, terms, arg) -> sum_terms !terms + sum_args [!arg]
    | E -> 0
  in
  count s

let nvars s = Sat.get_uid s

type name_map = (string * int) list M.t

let rec name_map vmap map s = 
  if s <> E && M.mem (Sat.get_uid s) vmap then vmap, map
  else
    let add vmap = M.add (Sat.get_uid s) 0 vmap in
    match s with
    | P(_ ) -> add vmap, map
    | C(_, _, args) -> 
      let vmap, map = 
        List.fold_left (fun (vmap,map) arg -> name_map vmap map arg) (vmap,map) args
      in
      add vmap, map
    | W(uid, names, _, arg) ->
      let map = if !names <> [] then M.add uid !names map else map in
      let vmap, map = name_map vmap map !arg in
      add vmap, map
    | E -> vmap, map

let name_map m s = 
  snd @@ name_map M.empty m s

let rec fold map f a s = 
  if s <> E && M.mem (Sat.get_uid s) map then map, a
  else
    let add map = M.add (Sat.get_uid s) 0 map in
    match s with
    | P(_, terms) -> 
      add map, f a terms
    | C(_, terms, args) -> 
      let a = f a terms in
      let map, a = List.fold_left (fun (map, a) t -> fold map f a t) (map, a) args in
      add map, a
    | W(_, _, terms, arg) -> 
      let a = f a !terms in
      let map, a = fold map f a !arg in
      add map, a
    | E -> map, a

let fold f a s = snd @@ fold M.empty f a s

module T = HardCaml.Transform.MakePureCombTransform(Comb)

let convert s = 
  let open HardCaml.Signal.Comb in
  if width s <> 1 then failwith "expecting single bit property"
  else
    let cnf = List.hd (T.rewrite_signals T.transform [s]) in
    relabel (List.hd cnf) 

let run ?(solver="dimacs-mini") cnf = 
  let module S = (val Sattools.Libs.get_solver solver) in
  let s = S.create () in
  S.add_clause s [ nvars cnf ];
  fold (fun () -> List.iter (S.add_clause s)) () cnf;
  let model = S.solve_with_model s in
  S.destroy s;
  model

let partition eq l =
  let rec f n p l = 
    match l with
    | [] -> [p]
    | h :: tl when eq h n -> f n (h :: p) tl
    | h :: tl             -> p :: f h [] l
  in
  match l with
  | [] -> []
  | h :: _ -> f h [] l

let to_vec l = 
  let len = 1 + List.fold_left (fun m (_,n,_) -> max m n) 0 l in
  let name = function (n,_,_) :: _ -> n |  [] -> failwith "bad vector" in
  let rec find_bit b = function 
    | [] -> None 
    | (_,b',v) :: tl -> if b'=b then Some(v) else find_bit b tl 
  in
  let value = String.init len 
    (fun i -> 
      match find_bit (len-i-1) l with 
      | None -> '?' 
      | Some(1) -> '1' 
      | _ -> '0')
  in
  name l, value

let report map = function
  | `unsat -> `unsat
  | `sat x ->
    let x = List.filter (fun x -> M.mem (abs x) map) x in
    let x = List.sort compare @@ List.flatten @@ List.map 
      (fun x -> 
        let l = M.find (abs x) map in
        List.map (fun (n,b) -> (n, b, if x<0 then 0 else 1)) l) 
      x 
    in
    let x = List.map to_vec @@ partition (fun (n,_,_) (m,_,_) -> n=m) x in
    `sat x

