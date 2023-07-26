exception NotImplemented;;

let rec forall : ('a -> bool) -> 'a list -> bool
= fun f lst -> 
    match lst with
      |[] -> true
      |hd::tl -> if f hd && forall f tl then true else false;;
      