(* test the logic for providing banned solutions *)

open HardCamlBloop
open Gates.Comb

let a, b = input "a" 4, input "b" 4
let c = (ue a) +: (ue b)

(* only solution is 15+15 *)
let () = Sat.(report @@ of_signal (c ==:. 30)) 

(* now ban the solution various ways *)
let () = Sat.(report @@ of_signal ~banned:[~: (a ^:. 15)] (c ==:. 30)) 
let () = Sat.(report @@ of_signal ~banned:[~: (b ^:. 15)] (c ==:. 30)) 
let () = Sat.(report @@ of_signal ~banned:[msb a] (c ==:. 30)) 

(* but this should be OK *)
let () = Sat.(report @@ of_signal ~banned:[~: (a ^:. 14)] (c ==:. 30)) 

(* again, but we need to ban a couple of solutons; 14+15 or 15+14 *)
let () = Sat.(report @@ of_signal (c ==:. 29))

(* get the other solution *)
let () = Sat.(report @@ of_signal 
                ~banned:[
                  ~: (a ^:. 14); 
                  ~: (b ^:. 15);
                ] 
                (c ==:. 29)) 

(* no solution *)
let () = Sat.(report @@ of_signal 
                ~banned:[
                  ~: (a ^:. 14); 
                  ~: (a ^:. 15); 
                  ~: (b ^:. 14);
                  ~: (b ^:. 15);
                ] 
                (c ==:. 29)) 

let () = Sat.(report @@ of_signal 
                ~banned:[
                  ~: (a ^:. 14); 
                  ~: (a ^:. 15); 
                ] 
                (c ==:. 29)) 

