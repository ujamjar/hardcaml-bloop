(* network repair example.
 * Given a known good circuit, and a circuit with a single bad gate, find the gate
 * which will repair the circuit.
 *
 * The idea is to replace the bad gate with a 4:1 mux, quantify away all the other
 * inputs, and solve for the mux values.  This provides the truth table of the gate
 * we want *)

open Printf

open HardCamlBloop
open Gates.Comb

(* inputs *)
let x' = input "x" 3 
let x = x' |> bits |> List.rev |> Array.of_list

(* model unknown gate *)
let d' = input "d" 4 
let d = d' |> bits |> List.rev |> Array.of_list
let (+?) a b = mux (b @ a) [ d.(0); d.(1); d.(2); d.(3)]

(* good version *)
let f = (x.(0) |: x.(1)) &: x.(2)

(* bad or gate *)
let g = (x.(0) +? x.(1)) &: x.(2)
let g = forall x' (f ==: g)

(* bad and gate *)
let h = (x.(0) |: x.(1)) +? x.(2)
let h = forall x' (f ==: h)

(* repair the or gate *)
let sat = Sat.(report (of_signal g))

(* repair the and gate *)
let sat = Sat.(report (of_signal h))

