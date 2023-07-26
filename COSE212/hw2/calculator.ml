exception NotImplemented;;

type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;
    
let rec xCal : int -> exp -> exp
= fun a e ->
    match e with
      |X -> INT a
      |INT n -> INT n
      |ADD (e1, e2) -> ADD (xCal a e1, xCal a e2)
      |SUB (e1, e2) -> SUB (xCal a e1, xCal a e2)
      |MUL (e1, e2) -> MUL (xCal a e1, xCal a e2)
      |DIV (e1, e2) -> DIV (xCal a e1, xCal a e2)
      |SIGMA (e1, e2, e3) -> SIGMA (xCal a e1, xCal a e2, xCal a e3)
      
  
let rec calculator : exp -> int
= fun exp -> 
    match exp with
      |X -> raise (Failure "Does not return Int")
      |INT n -> n
      |ADD (e1, e2) -> calculator e1 + calculator e2
      |SUB (e1, e2) -> calculator e1 - calculator e2
      |MUL (e1, e2) -> calculator e1 * calculator e2
      |DIV (e1, e2) -> calculator e1 / calculator e2
      |SIGMA (e1, e2, e3) -> sigma (calculator e1) (calculator e2) e3
      (*X�� ������ �� �ڸ��� Int n ���� ��ü�ؼ� calculator�� ���*)
      (*�ñ׸�1-10 �ñ׸��� ������ �ڿ� �ִ� �ñ׸� ���� �������ߵ�*)
and sigma : int -> int -> exp -> int
= fun n1 n2 e ->
    if n1 <= n2 then (calculator (xCal n1 e)) + (sigma (n1+1) n2 e) else 0;;
    