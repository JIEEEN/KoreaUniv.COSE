exception NotImplemented;;

type exp = 
  | V of var
  | P of var * exp
  | C of exp * exp
and var = string;;

let rec fnd : var list -> exp -> bool
= fun l e ->
    match l with
      |[] -> false
      |hd::tl -> 
        match e with
          |V v -> if hd = v || fnd tl e then true else false
          |P (va, ex) -> if fnd (l@[va]) ex then true else false
          |C (e1, e2) -> 
            match e1 with
              |V v -> if fnd l e2 then true else false
              |P (va, ex) -> if chk (l@[va]) ex then fnd (l@[va]) e2 else false
              |C (_e1, _e2) -> chk l _e1 && chk l _e2
and chk : var list -> exp -> bool
= fun l e ->
    match e with
      |V v -> fnd l e
      |P (va, ex) -> fnd (l@[va]) ex
      |C (e1, e2) -> fnd l e;;

let check : exp -> bool
= fun exp -> 
    match exp with
      |V v -> true
      |P (v, e) -> chk [v] e
      |C (e1, e2) -> chk [] exp;;