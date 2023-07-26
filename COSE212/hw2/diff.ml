exception NotImplemented;;

type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;
  
let rec sumDiff : aexp list -> string -> (aexp*string -> aexp) -> aexp  list
= fun l x f ->
    match l with
      |[] -> []
      |hd::tl -> [f (hd, x)]@(sumDiff tl x f);;

let rec timesDiff : aexp -> aexp list -> string -> (aexp*string -> aexp) -> aexp list
= fun a b x f ->
    [Times (f (a, x)::b); Times (a::[(f (Times b, x))])];;

let rec diff : aexp * string -> aexp
= fun (exp, x) ->
    match exp with
      |Const n -> Const 0
      |Var s -> if s = x then Const 1 else Var s
      |Power (s, n) -> if s = x then
        (
          match n with
            |0 -> Const 0
            |1 -> Const 1
            |a -> Times [Const a; Power(s, a-1)]
        )
        else Power(s, n)
      |Times l -> 
        (
          match l with
            |[] -> Const 0
            |hd::tl -> Sum (timesDiff hd tl x diff)
        )
      |Sum l ->
        Sum (sumDiff l x diff);;