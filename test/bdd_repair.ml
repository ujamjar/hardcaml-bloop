let show bdd = 
  let open Bdd in
  let () = Bdd.print_to_dot bdd "temp.dot" in
  ignore @@ Unix.system "dot -Tx11 temp.dot"

open HardCamlBloop
open Gates.Comb

(*
 
  input a b c d e f g h
  output Y_repair
  wires t1 t2 t3 t4 t5 t6 t7 w0 w1 w2 w3 w4 w5 w6 w7
  t1 = NOR(b, c)
  w0 = OR(f, g)
  t2 = INV(e)
  w1 = INV(h)
  w2 = NOR(c, g)
  w3 = OR(t1, w0)
  t3 = AND(t2, h)
  t4 = AND(w1, b)
  w4 = NOR(t3, t4)
  t5 = AND(d, w3)
  t6 = INV(w4)
  w5 = AND(t5, e)
  w6 = AND(t6, a)
  t7 = NOR(w6, w2)
  w7 = INV(t7)
  Y_repair = OR(w5, w7)

*)

let a, b, c, d, e, f, g, h = 
  let f x = input x 1 in
  f "a", f "b", f "c", f "d", f "e", f "f", f "g", f "h" 

let sel = input "sel" 3
let n = Array.init 8 (fun i -> sel ==:. i)

let y = 
  (a &: c &: ~: e &: g &: h) |: 
  (a &: b &: ~: h) |: 
  (d &: e &: ~: f &: ~: g) |: 
  (~: b &: ~: c &: d &: e) |: 
  (a &: ~: e &: h) |: 
  (~: c &: ~: g)

let _NOR(a,b) = ~: (a |: b)
let _OR(a,b) = (a |: b)
let _INV(a) = ~: a
let _AND(a,b) = a &: b

(* priority inverter - an xor gate
    i x o
    0 0 0
    0 1 1
    1 0 1
    1 1 0 *)
let pri i x = n.(i) ^: x

let t1 =       (_NOR(b, c))
let w0 = pri 0 (_OR(f, g))
let t2 =       (_INV(e))
let w1 = pri 1 (_INV(h))
let w2 = pri 2 (_NOR(c, g))
let w3 = pri 3 (_OR(t1, w0))
let t3 =       (_AND(t2, h))
let t4 =       (_AND(w1, b))
let w4 = pri 4 (_NOR(t3, t4))
let t5 =       (_AND(d, w3))
let t6 =       (_INV(w4))
let w5 = pri 5 (_AND(t5, e))
let w6 = pri 6 (_AND(t6, a))
let t7 =       (_NOR(w6, w2))
let w7 = pri 7 (_INV(t7))
let y_repair = (_OR(w5, w7))

let z = ~: (y ^: y_repair)

let z_quant = List.fold_right forall [a;b;c;d;e;f;g;h] z

open Expr
open Bddx
open Printf

(* BDD *)
let order = dynamic_weight_assignment [z_quant]
(*let order = vars_of_signal z_quant*)
let () = M.iter (fun v i -> printf "result: %s{%i}\n%!" (string_of_t v) i) order
let bdd = of_signal order z_quant
(*let () = List.iter show bdd*)

open HardCamlBloop
open Gates.Comb

let a, b, c0 = input "a" 4, input "b" 4, input "c0" 1
let c4_gold = msb ((ue a) +: (ue b) +: (uresize c0 5))

let c n cin = 
  (cin &: bit a n) |: (cin &: bit b n) |: (bit a n &: bit b n)

let c1 = c 0 c0
let c2 = c 1 c1
let c3 = c 2 c2
let c4 = c 3 c3

let d = input "d" 16 |> bits |> List.rev

let q = Array.init 4 (fun i -> bit a i ^: bit b i) |> Array.to_list |> List.rev |> concat

let r = mux q d

let cout = mux2 r c0 c4

let z = c4_gold ==: cout
let q = List.fold_right forall [a;b;c0] z

open Expr
open Bddx
open Printf

let () = printf "circuit built\n%!"
let order = dynamic_weight_assignment [q]
let () = printf "variables ordered\n%!"
(*let order = vars_of_signal q*)
let () = M.iter (fun v i -> printf "order: %s{%i}\n%!" (string_of_t v) i) order
let bdd = of_signal order q
let show_soln soln = printf "soln: %s\n%!" (string_of_t @@ term_of_solution order soln)
let () = List.iter (fun bdd -> List.iter show_soln (all_solutions bdd)) bdd
let () = List.iter show bdd


