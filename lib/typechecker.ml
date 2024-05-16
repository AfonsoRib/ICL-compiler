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


let eWithType e t n_e1 n_e2 =
  match e with
  |Add (_) ->  Add(n_e1, n_e2, t)
  |Sub (_) ->  Sub(n_e1, n_e2, t)
  |Mult (_) ->  Mult(n_e1, n_e2, t)
  |Div (_) ->  Div(n_e1, n_e2, t)
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
      | (IntType, FloatType) | (FloatType, IntType) | (FloatType, FloatType) -> (FloatType, eWithType e FloatType n_e1 n_e2)
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
  | Not (e1,_) -> let t = fst (typechecker e1 env)
                  in (t, Not(e1, t))
  | Let (binds, expr, _) ->
     let rec add_to_env (bindings : (string * exp * typ) list) (n_env : typ environment option) =
       match bindings with
       | [] -> []
       | (id, e1, t1)::rest -> let tpck = (typechecker e1 (ref n_env)) in
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
                                      (id,e1,v)::add_to_env rest n_env))
     in
     env := Env.begin_scope !env;
     let new_binds = add_to_env binds (!env) in
     let tpck = (typechecker expr env) in
     let res = fst tpck in
     let new_expr = snd tpck in
     env := Env.end_scope !env;
     (res, Let(new_binds, new_expr, res))
  | New(e1,_) ->  let t = RefType(fst (typechecker e1 env)) in
                  (t, New(e1, t))
  | Deref(e1,_) ->
     let t1 = fst (typechecker e1 env) in
     (match t1 with
      | RefType r -> (r, Deref(e1,r))
      | _ -> (NoneType, Deref(e1,NoneType)))
  | Assign(id,e1,_) -> if (Env.find !env id) = NoneType || fst (typechecker e1 env) = NoneType then (NoneType, Assign(id, e1, NoneType)) else
                         (UnitType, Assign(id,e, UnitType))
  | While (e1,e2,_) -> let t1 = fst (typechecker e1 env) and
                           t2 =  fst (typechecker e2 env) in
                       if t1 != BoolType || t2 = NoneType then (NoneType, While(e1,e2,NoneType)) else
                         (UnitType, While(e1,e2,UnitType))
  | IfThenElse (e1,e2,e3,_) -> let t1 = fst (typechecker e1 env) and
                                   t2 = fst (typechecker e2 env) and
                                   t3 = fst (typechecker e3 env)
                               in
                               if t1 != BoolType || t2 = NoneType || t3 = NoneType then (NoneType, IfThenElse(e1,e2,e3,NoneType)) else
                                 if t2 = t3 then (t2, IfThenElse(e1,e2,e3,t2)) else (NoneType, IfThenElse(e1,e2,e3,NoneType))
  | IfThen (e1,e2,_) -> let t1 = fst (typechecker e1 env) and
                            t2 = fst (typechecker e2 env)
                        in
                        if t1 != BoolType || t2 != UnitType then (NoneType, IfThen(e1,e2,NoneType)) else
                          (t2,IfThen(e1,e2,t2))
  | Seq(e1,e2,_) -> let e1type = fst( typechecker e1 env) in
                    let e2type = fst (typechecker e2 env) in
                    if e1type = NoneType || e2type = NoneType then (NoneType, Seq(e1,e2,NoneType)) else
                      (e2type, Seq(e1,e2,e2type))
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
