(* calculate tautology using positional cube notation *)

open Printf
open HardCamlBloop
open Expr

let a = var "a"
let b = var "b"

let tautology e = Pcn.(tautology @@ fst @@ build e)
let (==:) a b = ~: (a ^: b)

let () = printf "a | a' : %b\n" @@ tautology (a |: ~: a)
let () = printf "(a & a')' : %b\n" @@ tautology (~: (a &: ~: a))
(* demorgans law *)
let () = printf "(a | b) = (a' & b')' : %b\n" @@
  tautology ((a |: b) ==: ~: (~: a &: ~: b))
let () = printf "(a & b) = (a' | b')' : %b\n" @@
  tautology ((a &: b) ==: ~: (~: a |: ~: b))

