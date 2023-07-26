exception NotImplemented;;

let rec insert : int -> int list -> int list
= fun a l ->
    match l with
      |[] -> [a]
      |hd::tl -> if a > hd then hd::(insert a tl) else a::l;;

let rec sort : int list -> int list
= fun lst ->
    match lst with
      |[] -> []
      |hd::tl -> insert hd (sort tl);;
      
let rec reverse : int list -> int list
= fun lst -> 
    match lst with
      |[] -> []
      |hd::tl -> (reverse tl)@[hd];;
      
let min : int list -> int
= fun lst -> 
    let a=sort(lst) in
      match a with
        |[] -> raise (Failure "List is Empty!")
        |hd::tl -> hd;;       
        
let max : int list -> int
= fun lst -> 
    let a=reverse(sort(lst)) in
      match a with
        |[] -> raise (Failure "List is Empty!")
        |hd::tl -> hd;;