type uid = int
type mig = Pi of uid * string * int | T | M of uid * edge * edge * edge
and compl = NoCompl | Compl
and edge = mig * compl
and output = Po of edge

let uid = 
  let id = ref 0 in
  (fun () -> incr id; !id)

module Basic_gates = struct

  let t = (T, NoCompl)
  let f = (T, Compl)

  let (&.) a b = (M(uid(), a, b, f), NoCompl)
  let (|.) a b = (M(uid(), a, b, t), NoCompl)

  let (~.) (m, c) = 
    match c with
    | NoCompl -> (m, Compl)
    | Compl -> (m, NoCompl)

  let (^.) a b = ((~. a) &. b) |. (a &. (~. b))

end

module Base(Basic_gates : module type of Basic_gates) = struct
  open Basic_gates
  open List

  (* combinatorial API implemented using 'simple' gates *)

  type t = edge list

  let empty = []

  let gnd = [f]
  let vdd = [t]

  let width = List.length

  let const v = 
    let to_int = function '0' -> f | '1' -> t | _ -> failwith "invalid constant" in
    let len = String.length v in
    let rec const b i = 
      if len = i then b 
      else const (to_int v.[i] :: b) (i+1)
    in
    rev (const [] 0)

  let consti l v = const (HardCaml.Utils.bstr_of_int l v) 

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
      sum::res, carry) ([],f) (rev a) (rev b)))

  let (-:) a b = 
    let fs x y z = ((~. x) &. (y |. z)) |. (x &. y &. z), (x ^. y) ^. z in
    (fst (List.fold_left2 (fun (res,carry) a b -> 
      let carry, sum = fs a b carry in 
      sum::res, carry) ([],f) (rev a) (rev b)))
  
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
      | [],[] -> f
      | a::t0,b::t1 -> 
        mux2 (~. a &. b) t (mux2 (a &. ~. b) f (less t0 t1))
      | _ -> failwith "args not same width"
    in
    [less a b]

  let string_of_edge1 (m,c) = 
    match m with
    | Pi(uid,name,bit) -> name ^ "[" ^ string_of_int bit ^ "]"
    | T -> "1"
    | M(uid,_,_,_) -> string_of_int uid

  let string_of_mig = function
    | Pi(uid,name,bit) -> name ^ "[" ^ string_of_int bit ^ "]"
    | T -> "1"
    | M(_,a,b,c) -> "Maj(" ^ 
                      string_of_edge1 a ^ "," ^ 
                      string_of_edge1 b ^ "," ^ 
                      string_of_edge1 c ^ ")"  

  let string_of_edge (m,c) = 
    match (m,c) with
    | T,Compl -> "1"
    | T,NoCompl -> "0"
    | _,Compl -> string_of_mig m
    | _,NoCompl -> "!" ^ string_of_mig m


  let to_string s = 
    String.concat ""
      (mapi (fun i s -> "[" ^ string_of_int i ^ "] " ^ string_of_edge s ^ "\n") (rev s))

  let to_bstr _ = failwith "cannot do to_bstr"

  (** we need something to do inputs.  for now, we'll do it over the applied functor *)
  let (<==) a b = ()
  let (--) a b = a
  let wire w = empty 

end
