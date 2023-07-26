exception NotImplemented;;

let rec fold_right f l a 
= match l with
    |[] -> a
    |hd::tl -> f hd (fold_right f tl a);;
(*
let rec sum: int list -> int
= fun a ->
    match a with
      |[] -> 0
      |hd::tl -> hd+(sum tl);;
*)
let sum: int list -> int
= fun a -> fold_right (fun x y -> x+y) a 0;;

let rec suml: int list list -> int
= fun l ->
    match l with
      |[] -> 0
      |hd::tl -> (sum hd) + (suml tl);;