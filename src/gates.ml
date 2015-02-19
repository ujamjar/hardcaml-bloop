open HardCaml

let (&.) a b = 
  let open Expr in
  match a, b with
  | _, F 
  | F, _ -> F
  | _, T -> a
  | T, _ -> b
  | a, b when a=b -> a
  | Not(a),b when a=b -> F
  | a,Not(b) when a=b -> F
  | _ -> And(a,b)

let (|.) a b = 
  let open Expr in
  match a, b with
  | _, T 
  | T, _ -> T
  | _, F -> a
  | F, _ -> b
  | a, b when a=b -> a
  | Not(a),b when a=b -> T
  | a,Not(b) when a=b -> T
  | _ -> Or(a,b)

let (~.) a = 
  let open Expr in
  match a with
  | F -> T
  | T -> F
  | Not(a) -> a 
  | _ -> Not a

let (^.) a b = (~. a &. b) |. (a &. ~. b)

module Base = struct
  
  open List

  (* combinatorial API implemented using 'simple' gates *)

  type t = Expr.t list

  let empty = []

  let gnd = [Expr.F]
  let vdd = [Expr.T]

  let width = List.length

  let const v = 
    let to_int = function '0' -> Expr.F | '1' -> Expr.T | _ -> failwith "invalid constant" in
    let len = String.length v in
    let rec const b i = 
      if len = i then b 
      else const (to_int v.[i] :: b) (i+1)
    in
    rev (const [] 0)

  let consti l v = const (Utils.bstr_of_int l v) 

  let concat = concat

  let select s h l = 
    let rec sel b i = 
      match b with
      | [] -> []
      | hd::tl -> 
        if i > h then []
        else if i >= l then hd :: sel tl (i+1)
        else sel tl (i+1)
    in
    rev (sel (rev s) 0)

  let to_int _ = failwith "Can't convert simple gates to int"

  let assert_same_width l = 
    let w = width (hd l) in
    let _ = iter (fun b -> assert (w = width b)) l in
    w

  let (~:) a = map (~.) a

  let (|:) a b = 
    let _ = assert_same_width [a; b] in
    map2 (|.) a b
  
  let (&:) a b = 
    let _ = assert_same_width [a; b] in
    map2 (&.) a b
  
  let (^:) a b = 
    let _ = assert_same_width [a; b] in
    map2 (^.) a b

  let reduce f l = fold_left f (hd l) (tl l)

  let (==:) a b = [reduce (&.) (~: (a ^: b))]

  let rec repeat s n = 
    if n = 0 then empty
    else if n = 1 then s
    else concat [s; repeat s (n-1)]

  let mux sel vals =
    let last_elem = hd (rev vals) in
    Array.init (1 lsl (width sel))
      (fun i -> 
        let e = ((consti (width sel) i) ==: sel) in
        let v = try List.nth vals i with _ -> last_elem in
        let e = repeat e (width v) in
        e &: v) |> Array.to_list |> reduce (|:) 

    let (+:) a b = 
      let fa x y z = (x &. y) |. (x &. z) |. (y &. z), x ^. y ^. z in
      (fst (List.fold_left2 (fun (res,carry) a b -> 
        let carry, sum = fa a b carry in 
        sum::res, carry) ([],Expr.F) (rev a) (rev b)))

    let (-:) a b = 
      let fs x y z = ((~. x) &. (y |. z)) |. (x &. y &. z), (x ^. y) ^. z in
      (fst (List.fold_left2 (fun (res,carry) a b -> 
        let carry, sum = fs a b carry in 
        sum::res, carry) ([],Expr.F) (rev a) (rev b)))
    
    let zero = repeat gnd 
    let msb x = select x (width x - 1) (width x - 1)
    let bits = List.map (fun x -> [x])

    let ( *: ) a b = 
      let _,r = List.fold_left (fun (i,acc) b -> 
        let acc = concat [gnd; acc] in
        let a = concat [ gnd ; a ; repeat gnd i ] in
        i+1, (+:) acc ((&:) a (repeat b (width a)))
      ) (0,(zero (width a))) (rev (bits b)) in
      r
    
    let ( *+ ) a b = 
      let last = (width b) - 1 in
      let _,r = List.fold_left (fun (i,acc) b -> 
        let acc = concat [msb acc; acc] in
        let a = concat [ msb a; a ; repeat gnd i ] in
        i+1, (if i = last then (-:) else (+:)) acc ((&:) a (repeat b (width a)))
      ) (0,(zero (width a))) (rev (bits b)) in
      r
    
    let mux2 c a b = (c &. a) |. ((~. c) &. b)

    let (<:) a b =
      let rec less a b = 
        match a,b with
        | [],[] -> Expr.F
        | a::t0,b::t1 -> 
          mux2 (~. a &. b) Expr.T (mux2 (a &. ~. b) Expr.F (less t0 t1))
        | _ -> failwith "args not same width"
      in
      [less a b]

  let to_string s = 
    String.concat ""
      (mapi (fun i s -> "[" ^ string_of_int i ^ "] " ^ Expr.string_of_t s ^ "\n") (rev s))

  let to_bstr _ = failwith "cannot do to_bstr"

  (** we need something to do inputs.  for now, we'll do it over the applied functor *)
  let (<==) a b = ()
  let (--) a b = a
  let wire w = empty 

end

module Comb = struct
  module G' = HardCaml.Comb.Make(Base)
  include G'
  let input name bits = Array.init bits (fun i -> Expr.Var(name, (bits-i-1))) |> Array.to_list
end


