exception NotImplemented;;

let rec range : int -> int -> int list
= fun n1 n2 -> 
    if n1 > n2 then [] else n1::(range (n1+1) n2);;
