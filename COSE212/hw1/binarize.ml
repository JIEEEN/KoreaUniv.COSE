exception NotImplemented;;

let rec divide : int -> int
= fun x ->
    if x/2 > 0 then 2*(divide (x/2)) else 1;;

let rec bin : int -> int -> int list
= fun n a ->
    if a <> 0 then if (n - a) >= 0 then [1]@(bin (n-(divide n)) (a/2)) else [0]@(bin n (a/2)) else [];;
  
let binarize : int -> int list
= fun n -> 
    bin n (divide n);;
