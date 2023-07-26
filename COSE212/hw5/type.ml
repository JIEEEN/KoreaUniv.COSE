type program = exp
and exp =
    UNIT
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



type typ =
    TyUnit
  | TyInt
  | TyBool
  | TyFun of typ * typ
  | TyList of typ
  | TyVar of tyvar
and tyvar = string

type type_eqn = (typ * typ) list

exception TypeError

let rec list_map
= fun f l ->
    match l with
      |hd::tl -> [f hd]@(list_map f tl)
      |[] -> []

module TypeEnv = struct
  type t = var -> typ
  let empty = fun _ -> raise (TypeError)
  let extend (x, t) typEnv = 
    fun y -> 
      if x = y then t else (typEnv y)
  let find typEnv x = typEnv x
end

module Sub = struct
  type t = (tyvar * typ) list
  let empty = []
  let rec find : tyvar -> t -> typ 
  = fun x subst ->
    match subst with
      |(tv, tp)::tl -> if x = tv then tp else find x tl
      (*|[] -> raise (TypeError)*)
      |[] -> TyVar x
  let rec apply : typ -> t -> typ
  = fun typ subst ->
    match typ with
      |TyUnit -> TyUnit
      |TyInt -> TyInt
      |TyBool -> TyBool
      |TyFun (t1, t2) ->
        TyFun (apply t1 subst, apply t2 subst)
      |TyList t ->
        TyList (apply t subst)
      |TyVar x ->
        try find x subst with _ -> typ
        
  let extend : tyvar -> typ -> t -> t
  = fun typVar typ subst ->
    (typVar, typ)::(list_map (fun (x, y) -> (x, apply y [(typVar, typ)])) subst)
end


let equal_list : typ list ref = ref []

let rec equal_noError : Sub.t -> typ list -> bool
= fun subst equal_list->
    match equal_list with
      |hd::tl -> 
        begin
           match hd with
            |TyVar x ->
              begin
                match Sub.find x subst with 
                  |TyInt -> equal_noError subst tl
                  |TyBool -> equal_noError subst tl
                  |_ -> false
              end
            |_ -> raise (TypeError)
        end
      |[] -> true

(* generate a fresh type variable *)
let fresh_tyvar : unit -> typ = 
  let tyvar_num = ref 0 in
    fun () -> (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))
    

let rec v : TypeEnv.t -> exp -> typ -> type_eqn
= fun typEnv exp t -> (* { (x|->int), x+1, a } *)
    match exp with
      | UNIT -> [(t, TyUnit)]
      | TRUE -> [(t, TyBool)]
      | FALSE ->  [(t, TyBool)]
      | CONST n -> [(t, TyInt)]
      | VAR x -> [(t, TypeEnv.find typEnv x)]
      | ADD (e1, e2) -> [(t, TyInt)]@(v typEnv e1 TyInt)@(v typEnv e2 TyInt)
      | SUB (e1, e2) -> [(t, TyInt)]@(v typEnv e1 TyInt)@(v typEnv e2 TyInt)
      | MUL (e1, e2) -> [(t, TyInt)]@(v typEnv e1 TyInt)@(v typEnv e2 TyInt)
      | DIV (e1, e2) -> [(t, TyInt)]@(v typEnv e1 TyInt)@(v typEnv e2 TyInt)
      | EQUAL (e1, e2) -> 
        let new_typ = fresh_tyvar() in
          (equal_list := (new_typ)::(!equal_list)); ([(t, TyBool)]@(v typEnv e1 new_typ)@(v typEnv e2 new_typ))
        (*let new_typ = fresh_tyvar() in
          [(t, TyBool)]@(v typEnv e1 new_typ)@(v typEnv e2 new_typ)*)
      | LESS (e1, e2) -> [(t, TyBool)]@(v typEnv e1 TyInt)@(v typEnv e2 TyInt)
      | NOT e ->  [(t, TyBool)]@(v typEnv e TyBool)
      | NIL -> 
        let new_typ = fresh_tyvar() in
          [(t, TyList new_typ)]
      | CONS (e1, e2) ->
        let new_typ = fresh_tyvar() in
          [(t, TyList new_typ)]@(v typEnv e1 new_typ)@(v typEnv e2 (TyList new_typ))
      | APPEND (e1, e2) ->
        let new_typ = fresh_tyvar() in
          [(t, TyList new_typ)]@(v typEnv e1 (TyList new_typ))@(v typEnv e2 (TyList new_typ))
      | HEAD e ->
        let new_typ = fresh_tyvar() in
          [(t, new_typ)]@(v typEnv e (TyList new_typ)) 
        
      | TAIL e ->
        let new_typ = fresh_tyvar() in
          [(t, TyList new_typ)]@(v typEnv e (TyList new_typ))
      | ISNIL e ->
        let new_typ = fresh_tyvar() in
          [(t, TyBool)]@(v typEnv e (TyList new_typ))
      | IF (b1, e1, e2) ->
        (v typEnv b1 TyBool)@(v typEnv e1 t)@(v typEnv e2 t)
      | LET (var, e1, e2) ->
        (*begin
          match e1 with
            |PROC (_var, e) -> let new_typ = fresh_tyvar() in  
                              (v typEnv e1 new_typ)@(v typEnv e2 t)
            |_ -> let new_typ = fresh_tyvar() in
                    (v typEnv e1 new_typ)@(v (TypeEnv.extend (var, new_typ) typEnv) e2 t)
        end*)
        let new_typ = fresh_tyvar() in
                    (v typEnv e1 new_typ)@(v (TypeEnv.extend (var, new_typ) typEnv) e2 t)
      | LETREC (v1, v2, e1, e2) ->
        let new_typ1 = fresh_tyvar() in
          let new_typ2 = fresh_tyvar() in
            let new_env = TypeEnv.extend (v1, TyFun (new_typ1, new_typ2)) typEnv in
              let extended_env = TypeEnv.extend (v2, new_typ1) new_env in
                (v extended_env e1 new_typ2)@(v new_env e2 t)
                
        (*let new_typ1 = fresh_tyvar() in
          let new_typ2 = fresh_tyvar() in
            let new_env = TypeEnv.extend (v2, new_typ1) typEnv in
              let extended_env = TypeEnv.extend (v1, TyFun(new_typ1, new_typ2)) new_env in
                (v extended_env e1 new_typ2)@(v extended_env e2 t) *)
              
              
      | LETMREC ((v1, v2, e1), (v3, v4, e2), e3) ->
        (*let new_typ_f = fresh_tyvar() in
          let new_typ_g = fresh_tyvar() in
            let new_typEnv = TypeEnv.extend (v2, new_typ_f) typEnv in
              let extended_typEnv = TypeEnv.extend (v4, new_typ_g) new_typEnv in
                (v typEnv e1 new_typ_f)@(v typEnv e2 new_typ_g)@(v extended_typEnv e3 t) *)
        
        (*let new_typ_x = fresh_tyvar() in
        let new_typ_y = fresh_tyvar() in
          let new_typ_e1 = fresh_tyvar() in
          let new_typ_e2 = fresh_tyvar() in
            let new_typEnv1 = TypeEnv.extend (v2, new_typ_x) typEnv in
            let new_typEnv2 = TypeEnv.extend (v1, TyFun (new_typ_x, new_typ_e1)) new_typEnv1 in
              let new_typEnv3 = TypeEnv.extend (v4, new_typ_y) new_typEnv2 in
              let new_typEnv4 = TypeEnv.extend (v3, TyFun (new_typ_y, new_typ_e2)) new_typEnv3 in
                (v new_typEnv4 e1 new_typ_e1)@(v new_typEnv4 e2 new_typ_e2)@(v new_typEnv4 e3 t)*)
        
        let new_typ_x = fresh_tyvar() in
        let new_typ_y = fresh_tyvar() in
          let new_typ_e1 = fresh_tyvar() in
          let new_typ_e2 = fresh_tyvar() in
            let new_typEnv1 = TypeEnv.extend (v1, TyFun (new_typ_x, new_typ_e1)) typEnv in
            let new_typEnv2 = TypeEnv.extend (v3, TyFun (new_typ_y, new_typ_e2)) new_typEnv1 in
              let new_typEnv3 = TypeEnv.extend (v2, new_typ_x) new_typEnv2 in
              let new_typEnv4 = TypeEnv.extend (v4, new_typ_y) new_typEnv3 in
                (v new_typEnv4 e1 new_typ_e1)@(v new_typEnv4 e2 new_typ_e2)@(v new_typEnv2 e3 t)
                
      | PROC (var, e) ->
        let input = fresh_tyvar() in
          let output = fresh_tyvar() in
            [(t, TyFun (input, output))]@(v (TypeEnv.extend (var, input) typEnv) e output)
      | CALL (e1, e2) ->
        let input = fresh_tyvar() in
          (v typEnv e1 (TyFun (input, t)))@(v typEnv e2 input)
          
      | PRINT e -> [(t, TyUnit)]
      | SEQ (e1, e2) ->
        let new_typ = fresh_tyvar() in
          (v typEnv e1 new_typ)@(v typEnv e2 t)
      
and unify : (typ * typ) -> Sub.t -> Sub.t
= fun (t1, t2) subst ->
    match (t1, t2) with
      |(TyUnit, TyUnit) -> subst
      |(TyInt, TyInt) -> subst
      |(TyBool, TyBool) -> subst
      |(TyList a, TyList b) -> unify (Sub.apply a subst, Sub.apply b subst) subst
      |(TyFun (t1, t2), TyFun (t3, t4)) -> 
        let s1 = unify (t1, t3) subst in
          unify (Sub.apply t2 s1, Sub.apply t4 s1) s1
      |(TyVar x, _) ->  (*t2에서 _ 로 변경*) 
        begin
          match t2 with
            (*|TyBool -> if equal_error typEnv then raise (TypeError) else Sub.extend x t2 subst*)
            |(TyFun (_t1, _t2)) ->
              if (deep t1 t2) then raise (TypeError) else Sub.extend x t2 subst
            |TyVar y -> if (deep t1 t2) then subst else Sub.extend x t2 subst
            |_ -> Sub.extend x t2 subst
        end
      |(_t1, TyVar x) -> unify (t2, t1) subst
      |(_, _) -> raise (TypeError)
      
and deep : typ -> typ -> bool
= fun t1 t2 ->
    match t2 with
      |TyFun (_t1, _t2) -> deep t1 _t1 || deep t1 _t2
      |_ -> t1 == t2
            

let rec unify_all : type_eqn -> Sub.t -> Sub.t
= fun type_eqn subst ->
    match type_eqn with
      |(t1, t2)::tl -> 
        let new_subst = unify (Sub.apply t1 subst, Sub.apply t2 subst) subst in
          unify_all tl new_subst
      |[] -> subst;;
      
      

let typeof : exp -> typ
=fun e -> 
  let alpha = fresh_tyvar() in
    let typEnv = TypeEnv.empty in
      let typ_eqn = v typEnv e alpha in
        let subst = unify_all typ_eqn [] in 
          let flag = (equal_noError subst !equal_list) in
            if flag then (Sub.apply alpha subst) else raise(TypeError);;
          
   
  
  