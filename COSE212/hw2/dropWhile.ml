exception NotImplemented;;

let rec dropWhile : ('a -> bool) -> 'a list -> 'a list
= fun test lst -> 
    match lst with
      |[] -> []
      |hd::tl -> if test hd then dropWhile test tl else [hd]@(dropWhile test tl);;
      