exception NotImplemented;;

let rec pascal : int * int -> int
= fun (n, m) -> 
    if m > n then raise (Failure "Undefined Range Error")
      else if m=0 || m=n then 1 else pascal(n-1, m-1)+pascal(n-1, m);;
        