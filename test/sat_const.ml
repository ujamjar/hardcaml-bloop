(* add two inputs, compare to a number.  
 * Sat finds a valid input combination *)

open Printf

open HardCamlBloop
open Gates.Comb

let test_add_const n = 
  printf "a + b=%i? " n;
  (* add two numbers, compare to constant *)
  let a, b = input "a" 4, input "b" 4 in
  let c = (ue a) +: (ue b) in
  let f = c ==:. n in
  (* run sat *)
  Sat.report @@ Sat.of_signal f 

let () = test_add_const 3  (* should be sat *)
let () = test_add_const 22
let () = test_add_const 30
let () = test_add_const 31 (* should be unsat *)

