exception NotImplemented;;

type btree = 
  | Leaf of int
  | Left of btree
  | Right of btree
  | LeftRight of btree * btree;;

let rec mirror : btree -> btree
= fun tree -> (* ((1, 2), Nil) -> (Nil, (2, 1)) ·Î ÀüÈ¯) ((1, 2), Nil) -> Left(LeftRight(Leaf 1, Leaf 2)) *)
    match tree with
      |Leaf l -> Leaf l
      |LeftRight(t1, t2) -> LeftRight(mirror (t2), mirror (t1))
      |Left t -> Right (mirror t)
      |Right t -> Left (mirror t);;