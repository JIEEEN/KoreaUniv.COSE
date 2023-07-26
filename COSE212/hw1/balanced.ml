exception NotImplemented;;

type nat = ZERO | SUCC of nat;;

let rec cnt : nat -> int
= fun n ->
    match n with
      |ZERO -> 0
      |SUCC n1 -> (1 + cnt(n1));;
      
let rec gen : nat -> int -> int -> nat
= fun n x a->
    if a <> x then SUCC (gen (SUCC n) x (a+1)) else ZERO;;
    
let natadd : nat -> nat -> nat
= fun n1 n2 ->
    gen ZERO (cnt n1 + cnt n2) 0;; 
    

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
    gen ZERO (cnt n1 * cnt n2) 0;;