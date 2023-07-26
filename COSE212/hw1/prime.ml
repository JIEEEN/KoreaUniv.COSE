exception NotImplemented;;

let rec cnt : int -> int -> int
= fun n m ->
    match m with
      | 0 -> 0
      | _ -> if n mod m = 0 then 1 + cnt n (m-1) else cnt n (m-1);;
      
      
let prime : int -> bool
= fun n -> 
    if cnt n n = 2 then true else false;;