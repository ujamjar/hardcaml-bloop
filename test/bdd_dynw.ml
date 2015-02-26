(* dynamic weigth assignment example *)

open HardCamlBloop
open Expr

let a, b, c, d, e = var "a", var "b", var "c", var "d", var "e"

let g1 = a &: b
let g2 = (c &: d) &: b
let f = g1 |: g2
let g = d ^: e

open Printf
open Bddx

let order = dynamic_weight_assignment [[f];[g]]
let () = List.iter (fun (v,i) -> printf "result: %s{%i}\n" (string_of_t v) i) order


