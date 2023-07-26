exception NotImplemented;;

let rec range : int -> int -> int list
= fun n1 n2 -> 
    if n1 > n2 then [] else n1::(range (n1+1) n2);;
    
    
let rec reverse : 'a list -> 'a list -> 'a list
= fun lst l ->
    match lst with
      |[] -> l
      |hd::tl -> (reverse tl ([hd]@l));;
        
let fastrev : 'a list -> 'a list
= fun lst -> 
     reverse lst [];;