type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of var * var * exp *
                     var * var * exp * env
and env = (var * value) list

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f accu lst ->
  match lst with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f lst ->
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ fold_left (fun s x -> s ^ ", " ^ x) "" (map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

let op 
= fun f x y ->
    Int (f x y)

exception UndefinedSemantics

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl
  


let rec eval : exp -> env -> value
=fun exp env ->
  match exp with
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | _ -> 
    match exp with
      |UNIT -> Unit
      |TRUE -> Bool true
      |FALSE -> Bool false
      |CONST n -> Int n
      |VAR v -> lookup_env v env
      |ADD (e1, e2) -> 
        let x = eval e1 env in
          let y = eval e2 env in
            begin
            match (x, y) with
              |Int n1, Int n2 -> op (+) n1 n2
              |_ -> raise (UndefinedSemantics)
            end
      |SUB (e1, e2) -> 
        let x = eval e1 env in
          let y = eval e2 env in
            begin
            match (x, y) with
              |Int n1, Int n2 -> op (-) n1 n2
              |_ -> raise (UndefinedSemantics)
            end
      |MUL (e1, e2) -> 
        let x = eval e1 env in
          let y = eval e2 env in
            begin
            match (x, y) with
              |Int n1, Int n2 -> op ( * ) n1 n2
              |_ -> raise (UndefinedSemantics)
            end
      |DIV (e1, e2) -> 
        let x = eval e1 env in
          let y = eval e2 env in
            begin
            match (x, y) with
              |Int n1, Int n2 -> op (/) n1 n2
              |_ -> raise (UndefinedSemantics)
            end
      |EQUAL (e1, e2) -> 
        let x = eval e1 env in
          let y = eval e2 env in
            begin
            match (x, y) with
              |Bool b1, Bool b2 -> if b1 = b2 then Bool true else Bool false
              |Int n1, Int n2 -> if n1 = n2 then Bool true else Bool false
              |_ -> raise (UndefinedSemantics)
            end
      |LESS (e1, e2) -> 
        let x = eval e1 env in
          let y = eval e2 env in
            begin
            match (x, y) with
              |Int n1, Int n2 -> if n1 < n2 then Bool true else Bool false
              |_ -> raise (UndefinedSemantics)
            end
      |NOT e -> 
        let x = eval e env in
          begin
          match x with
            |Bool true -> Bool false
            |Bool false -> Bool true
            |_ -> raise (UndefinedSemantics)
          end
      |NIL -> List []
      |CONS (e1, e2) -> 
        let v1 = eval e1 env in
          let v2 = eval e2 env in
            begin 
            match v2 with 
              |List l -> List (v1::l)
              |_ -> raise (UndefinedSemantics)
            end
      |APPEND (e1, e2) ->
        let v1 = eval e1 env in
          let v2 = eval e2 env in
            begin
            match v1, v2 with
              |List l1, List l2 -> List (l1@l2)
              |_ -> raise (UndefinedSemantics)
            end
      |HEAD e ->
        let v = eval e env in
          begin
          match v with
            |List l ->
              match l with
                |[] -> List []
                |hd::tl -> hd
            |_ -> raise (UndefinedSemantics)
          end
      |TAIL e -> 
        let v = eval e env in
          begin
          match v with
            |List l ->
              match l with
                |[] -> List []
                |hd::tl -> List tl
            |_ -> raise (UndefinedSemantics)
          end
      |ISNIL e ->
        let v = eval e env in
          begin
          match v with
            |List l ->
              begin
              match l with
                |[] -> Bool true
                |hd::tl -> Bool false
              end
            |_ -> raise (UndefinedSemantics) 
          end
      |IF (e1, e2, e3) ->
        begin
        match eval e1 env with
          |Bool true -> eval e2 env
          |Bool false -> eval e3 env
          |_ -> raise (UndefinedSemantics)
        end
      |LET (v, e1, e2) ->
        let x = eval e1 env in
          eval e2 (extend_env (v, x) env)
      |LETREC (v1, v2, e1, e2) ->
        eval e2 (extend_env (v1, RecProcedure (v1, v2, e1, env)) env)
      |LETMREC ((v1, v2, e1), (v3, v4, e2), e3) -> 
        let x = extend_env (v1, MRecProcedure (v1, v2, e1, v3, v4, e2, env)) env in
          let y = extend_env (v3, MRecProcedure (v3, v4, e2, v1, v2, e1, env)) x in
            eval e3 y
      |PROC (v, e) ->
        Procedure (v, e, env)
      |CALL (e1, e2) ->
        (*e1계산하면 함수값, e2계산하면 value*)
        let x = eval e1 env in
          let y = eval e2 env in
            begin
            match x with
              |Procedure (v, e, p) ->
                let newEnv = extend_env (v, y) p in
                  eval e newEnv
              |RecProcedure (v1, v2, e, p) ->
                let newEnv = extend_env (v2, y) p in
                  let newEnv2 = extend_env (v1, RecProcedure (v1, v2, e, p)) newEnv in
                    eval e newEnv2
              |MRecProcedure (v1, v2, e1, v3, v4, e2, p) ->
                let newEnv = extend_env (v2, y) p in
                  let newEnv2 = extend_env (v1, MRecProcedure (v1, v2, e1, v3, v4, e2, env)) newEnv in
                    let newEnv3 = extend_env (v3, MRecProcedure (v3, v4, e2, v1, v2, e1, env)) newEnv2 in
                      eval e1 newEnv3
              |_ -> raise (UndefinedSemantics)
            end
      |SEQ (e1, e2) ->
        (eval e1 env); (eval e2 env)


let runml : program -> value
=fun pgm -> eval pgm empty_env;;