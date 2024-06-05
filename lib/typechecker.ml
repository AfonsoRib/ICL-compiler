open Ast
open Env
open Types

let rec typ_str typ_list =
  match typ_list with
  | [] -> failwith "no such type"
  | [t] -> (match t with
            | "int" -> IntType
            | "float" -> FloatType
            | "bool" -> BoolType
            | "unit" -> UnitType
            | "string" -> StringType
            | _  -> failwith "no such type ")
  | t::ts -> (match t with
              | "ref" -> RefType(typ_str ts)
              | _ -> failwith "no such type")

let rec str_typ = function
  | IntType -> "Int"
  | FloatType -> "Float"
  | BoolType -> "Bool"
  | UnitType -> "unit"
  | RefType t -> "Ref " ^ (str_typ t)
  | NoneType -> "None"
  | StringType -> "String"
  (* | FunType (args, ret) -> *)
  | _ -> failwith "not implemented. implement functions"

let eWithType e t n_e1 n_e2 =
  match e with
  |Add (_) ->  Add(n_e1, n_e2, t)
  |Sub (_) ->  Sub(n_e1, n_e2, t)
  |Mult (_) ->  Mult(n_e1, n_e2, t)
  |Div (_) ->  Div(n_e1, n_e2, t)
  |Addf (_) ->  Addf(n_e1, n_e2, t)
  |Subf (_) ->  Subf(n_e1, n_e2, t)
  |Multf (_) ->  Multf(n_e1, n_e2, t)
  |Divf (_) ->  Divf(n_e1, n_e2, t)
  |Fact (n, _) ->  Fact(n,t)
  |FloatFact (f,_) ->  FloatFact(f,t)
  |Eq (_) ->  Eq(n_e1, n_e2, t)
  |Ne (_) ->  Ne(n_e1, n_e2, t)
  |Le (_) ->  Le(n_e1, n_e2, t)
  |Ge (_) ->  Ge(n_e1, n_e2, t)
  |Lt (_) ->  Lt(n_e1, n_e2, t)
  |Gt (_) ->  Gt(n_e1, n_e2, t)
  |And (_) ->  And(n_e1, n_e2, t)
  |Or (_) ->  Or(n_e1, n_e2, t)
  |Not (e,_) ->  Not(e,t)
  |Statement (b, _) ->  Statement(b,t)
  |Let (bindings, e, _) ->  Let(bindings, e, t)
  |Id (x, _) ->  Id(x, t)
  |New (e, _) ->  New(e,t)
  |Deref (e,_) ->  Deref(e,t)
  |Assign (s,e,_) ->  Assign(s,e,t)
  |While (e1,e2,_) ->  While(e1,e2,t)
  |IfThenElse (e1,e2,e3,_) ->  IfThenElse(e1,e2,e3,t)
  |IfThen (e1,e2,_) ->  IfThen(e1,e2,t)
  |PrintLn (e1,_) ->  PrintLn(e1,t)
  |Print (e1,_) ->  Print(e1,t)
  |Seq (e1,e2,_) ->  Seq(e1,e2,t)
  |UnitExp (_) ->  UnitExp(t)
  |String (s,_) ->  String(s,t)
  |Fun (args,e1,_) -> Fun (args,e1,t)
  |App(e1,args, _) -> App(e1,args,t)

let rec typechecker (e : Ast.exp) (env : typ environment option ref): (typ * Ast.exp) =
  match e with
  | Fact (n, _) ->  (IntType, Fact(n,IntType))
  | FloatFact (n,_) -> (FloatType, FloatFact(n,FloatType))
  | Statement (b,_) -> (BoolType, Statement(b, BoolType))
  | Id (x, _) -> let t = Env.find !env x
                 in
                 (t, Id(x, t))
  | Add (e1, e2, _) | Mult (e1, e2,_) | Sub (e1, e2,_) | Div (e1, e2,_) ->
     let typecheck1 = typechecker e1 env and
         typecheck2 = typechecker e2 env in
     let
       t1 = fst typecheck1 and
       t2 = fst typecheck2 and
       n_e1 = snd typecheck1 and
       n_e2 = snd typecheck2
     in
     (match (t1,t2) with
      | (IntType, IntType) -> (IntType, eWithType e IntType n_e1 n_e2)
      | _ -> (NoneType, (eWithType e NoneType e1 e2)))
  | Addf (e1, e2, _) | Multf (e1, e2,_) | Subf (e1, e2,_) | Divf (e1, e2,_) ->
     let typecheck1 = typechecker e1 env and
         typecheck2 = typechecker e2 env in
     let
       t1 = fst typecheck1 and
       t2 = fst typecheck2 and
       n_e1 = snd typecheck1 and
       n_e2 = snd typecheck2
     in
     (match (t1,t2) with
      | (FloatType, FloatType) -> (FloatType, eWithType e FloatType n_e1 n_e2)
      | _ -> (NoneType, (eWithType e NoneType e1 e2)))
  | Ne (e1, e2, _)| Le (e1, e2, _)| Ge (e1, e2, _)| Lt (e1, e2, _)| Gt (e1, e2, _) | Eq (e1, e2, _) ->
     let rec aux t1 t2 n_e1 n_e2 =
       (match (t1,t2) with
        | (IntType, IntType) | (FloatType, FloatType) | (StringType, StringType) | (BoolType,BoolType) -> (BoolType, eWithType e BoolType n_e1 n_e2)
        | (RefType r1, RefType r2) -> aux r1 r2 n_e1 n_e2
        | _ -> (NoneType, eWithType e NoneType n_e1 n_e2))
     in
     let typecheck1 =  typechecker e1 env and
         typecheck2 = typechecker e2 env in
     let
       t1 = fst typecheck1  and
       t2 = fst typecheck2 and
       n_e1 = snd typecheck1 and
       n_e2 = snd typecheck2 
     in
     aux t1 t2 n_e1 n_e2
  | And (e1, e2, _) | Or (e1, e2, _) ->
     let typecheck1 =  typechecker e1 env and
         typecheck2 = typechecker e2 env in
     let
       t1 = fst typecheck1  and
       t2 = fst typecheck2 and
       n_e1 = snd typecheck1 and
       n_e2 = snd typecheck2 
     in
     (match (t1, t2) with
      | (BoolType, BoolType) -> (BoolType, eWithType e BoolType n_e1 n_e2)
      | _ -> (NoneType, eWithType e NoneType e1 e2))
  | Not (e1,_) -> let tpck1 = (typechecker e1 env) in
                  let t = fst tpck1 and
                      n_e1 = snd tpck1
                  in (t, Not(n_e1, t))
  | Let (binds, expr, _) ->
     let rec add_to_env (bindings : (string * exp * typ) list) (n_env : typ environment option) =
       match bindings with
       | [] -> []
       | (id, e1, t1)::rest -> 
        (match t1 with
        | FunType (_, _) ->
          Env.bind n_env id t1;
        | _ -> ());
        let tpck = (typechecker e1 (ref n_env)) in
                               let v = fst tpck and n_e1 = snd tpck in
                               (match t1 with
                                | NoneType ->
                                   Env.bind n_env id v;
                                   (id,n_e1,v)::add_to_env rest n_env
                                | _ ->
                                   if (t1 <> v) then
                                     failwith ("for id \"" ^ id ^ "\" types don't match")
                                   else
                                     (Env.bind n_env id v;
                                      (id,n_e1,v)::add_to_env rest n_env))
     in
     env := Env.begin_scope !env;
     let new_binds = add_to_env binds (!env) in
     let tpck = (typechecker expr env) in
     let res = fst tpck in
     let new_expr = snd tpck in
     env := Env.end_scope !env;
     (res, Let(new_binds, new_expr, res))
  | New(e1,_) -> let tpck = typechecker e1 env in
                 let n_e1 = snd tpck in
                 let t = RefType(fst tpck) in
                 (t, New(n_e1, t))
  | Deref(e1,_) ->
     let resolveRef t =
       match t with
       | RefType r -> r
       | _ -> failwith "Not a reference in deref "
     in
     let tpck = typechecker e1 env in
     let t1 = resolveRef(fst tpck) and
         n_e1 = snd tpck in
     (t1, Deref(n_e1,t1))
  | Assign(id,e1,_) -> let tpck = (typechecker e1 env) in
                       let tpckid =  typechecker id env in
                       if (fst tpckid) = NoneType || (fst tpck) = NoneType then (NoneType, Assign(id, e1, NoneType)) else
                         (UnitType, Assign(snd tpckid,snd tpck, UnitType)) (*shouldn't this be int?*)
  | While (e1,e2,_) -> let tpck1 = typechecker e1 env in
                       let tpck2 = typechecker e2 env
                       in
                       let t1 = fst tpck1 and
                           t2 = fst tpck2 and
                           n_e1 = snd tpck1 and
                           n_e2 = snd tpck2 in
                       if t1 != BoolType || t2 = NoneType then (NoneType, While(n_e1,n_e2,NoneType)) else
                         (UnitType, While(n_e1,n_e2,UnitType))
  | IfThenElse (e1,e2,e3,_) ->
     let 
       tpck1 = typechecker e1 env and
       tpck2 = typechecker e2 env and
       tpck3 = typechecker e3 env in
     let
       t1 = fst (tpck1) and
       t2 = fst (tpck2) and
       t3 = fst (tpck3) and
       n_e1 = snd tpck1 and
       n_e2 = snd tpck2 and
       n_e3 = snd tpck3 
     in
     if t1 != BoolType || t2 = NoneType || t3 = NoneType then (NoneType, IfThenElse(n_e1,n_e2,n_e3,NoneType)) else
       if t2 = t3 then (t2, IfThenElse(n_e1,n_e2,n_e3,t2)) else (NoneType, IfThenElse(n_e1,n_e2,n_e3,NoneType))
  | IfThen (e1,e2,_) -> let
      tpck1 = (typechecker e1 env) and
      tpck2 = (typechecker e2 env) in
    let t1 = fst tpck1  and
        t2 = fst tpck2 and
        n_e1 = snd tpck1 and
        n_e2 = snd tpck2
    in
    if t1 != BoolType || t2 != UnitType then (NoneType, IfThen(n_e1,n_e2,NoneType)) else
      (t2,IfThen(n_e1,n_e2,t2))
  | Seq(e1,e2,_) -> let tp1 = typechecker e1 env and
                        tp2 = (typechecker e2 env) in
                    let e1type = fst tp1 and
                        e2type = fst  tp2 and
                        n_e1 = snd tp1 and
                        n_e2 = snd tp2 
                    in
                    if e1type = NoneType || e2type = NoneType then (NoneType, Seq(e1,e2,NoneType)) else
                      (e2type, Seq(n_e1,n_e2,e2type))
  | PrintLn(e1,_) -> let typecheck1 = (typechecker e1 env) in
                     let t1 = fst typecheck1 and
                         n_e1 = snd typecheck1 in
                     if t1 = NoneType then (NoneType,PrintLn(n_e1,NoneType)) else
                       (UnitType, PrintLn (n_e1,UnitType))
  | Print(e1,_) -> let typecheck1 = (typechecker e1 env) in
                   let t1 = fst typecheck1 and
                       n_e1 = snd typecheck1 in
                   if t1 = NoneType then (NoneType,Print(n_e1,NoneType)) else
                     (UnitType, Print(n_e1,UnitType))
  | UnitExp _-> (UnitType, UnitExp(UnitType))
  | String(s,_) -> (StringType, String(s, StringType))
  | Fun (args,e1,_) ->
     let env0 = ref (Env.begin_scope !env) in
     let rec bind_types lst =
       match lst with
       | [] -> ()
       | (b,t)::bs -> 
          Env.bind !env0 b t; bind_types bs
     in
     let rec get_types lst =
       match lst with
       | [] -> []
       | (_,t)::ts -> t :: get_types ts
     in
     bind_types args;
     let tpck1 = typechecker e1 env0 in
     let t = fst tpck1 and
         n_e1 = snd tpck1
     in
     let ts = get_types args in     
     env0 := Env.end_scope !env0;
     if(t == NoneType) then
       (t,Fun(args,n_e1,t))
     else
       (FunType(ts,t),Fun(args,n_e1,FunType(ts,t)))
  | App(e1,args, _) ->
     let tpck = typechecker e1 env in
     let f_type = fst tpck in
     let rec typecheckArgs lst =
       match lst with
       | [] -> []
       | e::es ->  typechecker e env :: typecheckArgs es 
     in                  
     let rec verify_args lst1 lst2 ret=
       match lst1,lst2 with
       | [],[] -> ret
       | t1::t1s, t2::t2s ->
          let t2_type = fst (t2) in
          (* print_endline ("app_arg " ^ (Ref.string_of_type t2_type)); *)
          if t1 = t2_type then verify_args t1s t2s ret else NoneType
       | _ -> NoneType (* failwith "function was given more or is missing more arguments than supposed" *)
     in
     let typecheckedArgs = typecheckArgs args in
     match f_type with
     | FunType(f_args, e_type) ->
        (* List.iter (fun x -> print_endline ("farg " ^ Ref.string_of_type x)) f_args; *)
        (* List.iter (fun x -> print_endline ("app-args " ^ Ref.string_of_type x)) verify_args; *)
        (* print_endline ("fun_type " ^ (Ref.string_of_type e_type)); *)
        (* print_endline ("app_type " ^(Ref.string_of_type f_type)); *)
        let retArgs =  List.map snd typecheckedArgs in
        if (verify_args f_args typecheckedArgs e_type) = e_type then
        (e_type, App((snd tpck),retArgs, e_type))
        else
          (NoneType, App(e1,args, NoneType))
     | _ -> (NoneType, App(e1,args, NoneType))

