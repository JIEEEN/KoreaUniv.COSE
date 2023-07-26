exception NotImplemented;;

let rec lenOfInt : int -> int
= fun n ->
    match n with
      |0 -> 0
      |_ -> if (n/10) > 0 then 1 + (lenOfInt (n/10)) else 1;;

let rec len : int list -> int
= fun lst ->
    match lst with
      |[] -> 0
      |hd::tl -> (lenOfInt hd)+(len tl);;
      
let rec prd : int -> int
= fun x -> if x>0 then 10*prd(x-1) else 1;;

let rec lst2int : int list -> int
= fun lst -> 
    match lst with
      |[] -> 0
      |hd::tl -> hd*(prd (len tl)) + (lst2int tl);;