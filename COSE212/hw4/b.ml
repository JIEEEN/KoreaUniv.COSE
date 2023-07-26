type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp                 (* sequence *)
  | IF of exp * exp * exp            (* if-then-else *)
  | WHILE of exp * exp               (* while loop *)
  | LETV of id * exp * exp           (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list           (* call by value *)
  | CALLR of id * id list            (* call by referenece *)
  | RECORD of (id * exp) list        (* record construction *)
  | FIELD of exp * id                (* access record field *)
  | ASSIGN of id * exp               (* assgin to variable *)
  | ASSIGNF of exp * id * exp        (* assign to record field *)
  | WRITE of exp
and id = string

type loc = int
type value =
| Num of int
| Bool of bool
| Unit
| Record of record 
and record = (id * loc) list
type memory = (loc * value) list
type env = binding list
and binding = LocBind of id * loc | ProcBind of id * proc
and proc = id list * exp * env

(********************************)
(*     Handling environment     *)
(********************************)

let rec lookup_loc_env : id -> env -> loc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind (id,l) -> if(x=id) then l else lookup_loc_env x tl
    | ProcBind _ -> lookup_loc_env x tl
    end

let rec lookup_proc_env : id -> env -> proc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind _ -> lookup_proc_env x tl
    | ProcBind (id,binding) -> if (x=id) then binding else lookup_proc_env x tl
    end

let extend_env : binding -> env -> env
= fun e env -> e::env

let empty_env = []



(***************************)
(*     Handling memory     *)
(***************************)

let rec lookup_mem : loc -> memory -> value
= fun l mem ->
  match mem with
  | [] -> raise(Failure ("location "^(string_of_int l)^" is not included in memory"))
  | (loc,v)::tl -> if(l=loc) then v else lookup_mem l tl

let extend_mem : (loc * value) -> memory -> memory
= fun (l,v) mem -> (l,v)::mem

let empty_mem = []

(***************************)
(*     Handling record     *)
(***************************)

let rec lookup_record : id -> record -> loc
= fun id record -> 
  match record with
    | [] -> raise(Failure ("field "^ id ^" is not included in record"))
    | (x,l)::tl -> if(id=x) then l else lookup_record id tl


let extend_record : (id * loc) -> record -> record
= fun (x,l) record -> (x,l)::record

let empty_record = []

(***************************)

let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics

let rec list_fold2 : ('a -> 'b -> 'c -> 'c)-> 'a list -> 'b list -> 'c -> 'c
= fun func l1 l2 acc ->
  match (l1,l2) with
  | ([],[]) -> acc
  | (hd1::tl1,hd2::tl2) -> list_fold2 func tl1 tl2 (func hd1 hd2 acc)
  | _ -> raise (Failure "two lists have different length")

let rec list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun func l acc ->
  match l with
  | [] -> acc
  | hd::tl -> list_fold func tl (func hd acc)

let value2str : value -> string
= fun v ->
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "unit"
  | Record _ -> "record" 

let rec eval_aop : env -> memory -> exp -> exp -> (int -> int -> int) -> (value * memory)
= fun env mem e1 e2 op ->
  let (v1,mem1) = eval env mem e1 in
  let (v2,mem2) = eval env mem1 e2 in
  match (v1,v2) with
  | (Num n1, Num n2) -> (Num (op n1 n2), mem2)
  | _ -> raise (Failure "arithmetic operation type error")

and eval : env -> memory -> exp -> (value * memory) 
=fun env mem e -> 
  match e with
  | WRITE e -> 
    let (v1,mem1) = eval env mem e in
    let _ = print_endline(value2str v1) in
    (v1,mem1)
  | NUM n ->
    (Num n, mem)
  | TRUE -> (Bool true, mem)
  | FALSE -> (Bool false, mem)
  | UNIT -> (Unit, mem)
  | VAR id -> (* Error *)
    (lookup_mem (lookup_loc_env id env) mem, mem)
  | ADD (e1, e2) -> (*e1과 e2가 수인지 확인*)
    eval_aop env mem e1 e2 (fun x y -> x + y)
  | SUB (e1, e2) ->
    eval_aop env mem e1 e2 (fun x y -> x - y)
  | MUL (e1, e2) ->
    eval_aop env mem e1 e2 (fun x y -> x * y)
  | DIV (e1, e2) ->
    let (v, m) = eval env mem e2 in
      begin 
        match v with
          |Num 0 -> raise (UndefinedSemantics)
          |_ -> eval_aop env mem e1 e2 (fun x y -> x / y)
      end
  | EQUAL (e1, e2) ->
    let (v1, m1) = eval env mem e1 in
      let (v2, m2) = eval env m1 e2 in
      begin
        match (v1, v2) with
          | (Num val1, Num val2) -> if val1 = val2 then (Bool true, m2) else (Bool false, m2)
          | (Bool val1, Bool val2) -> if val1 = val2 then (Bool true, m2) else (Bool false, m2)
          | _ -> raise (UndefinedSemantics)
      end
  | LESS (e1, e2) ->
    let (v1, m1) = eval env mem e1 in
      let (v2, m2) = eval env m1 e2 in
      begin
        match (v1, v2) with
          | (Num val1, Num val2) -> if val1 < val2 then (Bool true, m2) else (Bool false, m2)
          | _ -> raise (UndefinedSemantics)
      end
  | NOT e -> 
    let (v, m) = eval env mem e in
      begin
        match v with
          |Bool true -> (Bool false, m)
          |Bool false -> (Bool true, m)
          |_ -> raise (UndefinedSemantics)
      end
  | SEQ (e1, e2) ->
    let (v1, m1) = eval env mem e1 in
      let (v2, m2) = eval env m1 e2 in
        (v2, m2)
  | IF (e1, e2, e3) ->
    let (v1, m1) = eval env mem e1 in
      begin
        match v1 with
          |Bool true -> (eval env m1 e2)
          |Bool false -> (eval env m1 e3)
          |_ -> raise (UndefinedSemantics)
      end
  | WHILE (e1, e2) ->
    let (v1, m1) = eval env mem e1 in
      begin
        match v1 with
          |Bool true ->
            let (v2, m2) = eval env m1 e2 in
              let (v3, m3) = eval env m2 (WHILE (e1, e2)) in
                (v3, m3)
          |Bool false -> (Unit, m1)
          |_ -> raise (UndefinedSemantics)
      end
  | LETV (id, e1, e2) ->           (* variable binding *)
    let (v1, m1) = eval env mem e1 in
      (* Dom에 존재하면 안됨 *)
      let loc = new_location() in 
        let new_env = extend_env (LocBind (id, loc)) env in
          let new_mem = extend_mem (loc, v1) m1 in
            eval new_env new_mem e2
  | LETF (id, idl, e1, e2) -> (* procedure binding *)
    let proc = (idl, e1, env) in
      eval (extend_env (ProcBind (id, proc)) env) mem e2
            


  | CALLV (id, expl) ->
    let (_idl, _exp, _env) = lookup_proc_env id env in
      let new_env = get_new_env _idl _env in
        let (new_mem, value_list) = get_new_mem env mem expl [] in
          let extended_env = extend_env (ProcBind (id, (_idl, _exp, _env))) new_env in
            let extended_mem = list_fold2 (fun x y z -> extend_mem (lookup_loc_env x new_env, y) z) _idl value_list new_mem in
              eval extended_env extended_mem _exp



    
  | CALLR (id, idl) ->  
    let (_idl, _exp, _env) = lookup_proc_env id env in
      let location_list = list_fold (fun x y -> y@[lookup_loc_env x env]) idl [] in
        let new_env = list_fold2 (fun x y z -> extend_env (LocBind (y, x)) z) location_list _idl _env in
          let extended_env = extend_env (ProcBind (id, (_idl, _exp, _env))) new_env in
            eval extended_env mem _exp
        

        
  | ASSIGN (id, e) ->
    let (v1, m1) = eval env mem e in
      let new_mem = extend_mem (lookup_loc_env id env, v1) m1 in
        (v1, new_mem)
  | RECORD idel -> (* (id*exp) list*)
    let new_rec = empty_record in
      eval_rec env mem new_rec idel
  | FIELD (e, id) -> 
    let (v, m) = eval env mem e in
      begin 
        match v with 
          |Record r ->
            let loc = lookup_record id r in
              (lookup_mem loc m, m)
          |_ -> raise (UndefinedSemantics)
      end
  | ASSIGNF (e1, id, e2) -> 
    let (v1, m1) = eval env mem e1 in
      let (v2, m2) = eval env m1 e2 in
        begin 
          match v1 with 
            |Record r ->
              let loc = lookup_record id r in
                (v2, extend_mem (loc, v2) m2)
            |_ -> raise (UndefinedSemantics)
        end
      
and get_new_env
= fun idl env ->
    match idl with
      |hd::tl ->  
        let loc = new_location() in
          let new_env = extend_env (LocBind(hd, loc)) env in
            get_new_env tl new_env
      |[] -> env
        
and get_new_mem
= fun env mem expl empty_list->
  match expl with
    |hd::tl -> 
      let (v, m) = eval env mem hd in
        let empty_list = (empty_list)@[v] in
          get_new_mem env m tl empty_list
    |[] -> (mem, empty_list)

and eval_rec : env -> memory -> record -> (id * exp) list -> (value * memory)
= fun env mem r e -> 
    match e with
      |(_id, _exp)::tl -> 
        let loc = new_location() in
          let (v, m) = eval env mem _exp in
            eval_rec env (extend_mem (loc, v) m) (extend_record (_id, loc) r) tl
      |[] -> (Record r, mem)

        

let runb : exp -> value 
=fun exp -> let (v, _) = eval empty_env empty_mem exp in v;;