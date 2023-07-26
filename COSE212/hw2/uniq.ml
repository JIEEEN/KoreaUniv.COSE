exception NotImplemented;;

let rec fnd : 'a list -> 'a -> bool
= fun lst a ->
    match lst with
      |[] -> false
      |hd::tl -> if a = hd then true else fnd tl a;;
      
let rec del
= fun lst l ->
    match lst with
      |[] -> l
      |hd::tl -> if fnd l hd then del tl l else del tl (l@[hd]);;
      
let uniq : 'a list -> 'a list
= fun lst -> 
    del lst [];;