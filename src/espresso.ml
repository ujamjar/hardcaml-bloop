(* espresso algorithm *)

module I = Set.Make(struct type t = int let compare = compare end)

module Expand = struct

  let expand f = 
    let f' = Pcn.(~: f) in
    f'

end

module Reduce = struct

  let to_bits = function
    | Pcn.T -> 1
    | Pcn.F -> 2
    | Pcn.X -> 3

  let of_bits = function
    | 1 -> Pcn.T
    | 2 -> Pcn.F
    | 3 -> Pcn.X
    | _ -> failwith "not sure what to do with this pcn cube value..."

  (* XXX shockingly inefficient...fixme *)
  let bits_of_cube c = 
    List.map to_bits @@ Pcn.Cube.to_list c

  let reduce_cube cl c = 
    let cl = Pcn.Cubelist.to_list Pcn.(~: cl) in
    let cl = 
      List.fold_left 
        (fun acc c' ->
          match Pcn.Cube.intersect c c' with
          | None -> acc
          | Some(c) -> 
              (match acc with
              | None -> Some c
              | Some(acc) -> Some(Pcn.Cube.supercube acc c)))
        None cl
    in
    match cl with None -> c | Some(c) -> c

  let reduce f = 
    let open Pcn.Cubelist in
    let rec g checked unchecked = 
      if num_cubes unchecked = 0 then checked
      else
        let c, unchecked = choose unchecked in
        let f' = Pcn.(concat [ checked; unchecked ]) in
        let c = reduce_cube f' c in
        g (add c checked) unchecked
    in
    g zero f

end

module Irredundant = struct
  
  let cofactor_by_cube f c = 
    fst @@ Pcn.Cube.fold (fun (cof,i) ->
      function Pcn.T -> Pcn.Cubelist.positive_cofactor i cof, i+1
             | Pcn.F -> Pcn.Cubelist.negative_cofactor i cof, i+1
             | Pcn.X -> cof, i+1) (f,0) c

  let irredundant cubes = 
    let open Pcn.Cubelist in
    let rec g checked unchecked = 
      if num_cubes unchecked = 0 then checked
      else
        let c,cubes = choose unchecked in 
        if Pcn.tautology (cofactor_by_cube (concat [checked; cubes]) c) then
          g checked cubes
        else
          g (add c checked) cubes
    in
    g zero cubes

end

(* need some algorithms to solve the covering problem *)
module Cover = struct

  let row_of_list l = snd @@ List.fold_left 
    (fun (i,s) x -> i+1,(if x then I.add i s else s)) (0,I.empty) l

  (* add s to cur and return overall size *)
  let count cur s = 
    let t = I.union cur s in
    I.cardinal t, I.cardinal (I.inter s t)

  (* sort by a>b, then x<y *)
  let compare_count (a,x) (b,y) = 
    let c = compare b a in
    if c<>0 then c
    else compare x y

  (* find best row to add, return it and the rest of the list *)
  let rec find_best_row cur l = 
    let l = List.map (fun (i,x) -> count cur x, (i, x)) l in (* find count metrics *)
    let l = List.sort (fun (a,(_)) (b,(_)) -> compare_count a b) l in (* sort by count *)
    snd (List.hd l), List.map snd (List.tl l) (* return best, and rest *)

  let find rows = 
    (* add row index which we need to track *)
    let rows = List.mapi (fun i r -> i, r) rows in

    (* size of the complete cover, as it exists in the rows *)
    let full_cover = I.cardinal @@ List.fold_left  (fun s (_,x) -> I.union s x) I.empty rows in

    (* search for cover *)
    let rec find_cover cur idx rest = 
        if rest = [] then 
          failwith "find_cover: no cover found; should not get here"
        else
          (* find the row which covers the most uncovered elements *)
          let (i, n), l = find_best_row cur rest in
          let cur, idx = I.union n cur, i :: idx in
          if I.cardinal cur <> full_cover then find_cover cur idx l
          else idx            
    in
    
    find_cover I.empty [] rows

end

module Example = struct

  let expand_cube f' c = ()

  let expand f = 
    (*let f' = Pcn.(~: f) in*)
    ()

end

