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

let rec typechecker (e : Ast.exp) (env : typ environment option ref): typ =
  match e with
  | Fact (_) ->  IntType
  | FloatFact (_) -> FloatType
  | Statement (_) -> BoolType
  | Id (x, _) -> Env.find !env x
  | Add (e1, e2, _) | Mult (e1, e2,_) | Sub (e1, e2,_) | Div (e1, e2,_) ->
     let t1 = (typechecker e1 env) and
         t2 = typechecker e2 env
     in
     (match (t1,t2) with
     | (IntType, IntType) -> IntType
     | (IntType, FloatType) -> FloatType
     | (FloatType, IntType) -> FloatType
     | (FloatType, FloatType) -> FloatType
     | _ -> NoneType)
  | Ne (e1, e2, _)| Le (e1, e2, _)| Ge (e1, e2, _)| Lt (e1, e2, _)| Gt (e1, e2, _) | Eq (e1, e2, _) ->
     let rec aux t1 t2 =
       (match (t1,t2) with
        | (IntType, IntType) | (FloatType, FloatType) | (StringType, StringType) | (BoolType,BoolType) -> BoolType
        | (RefType r1, RefType r2) -> aux r1 r2
        | _ -> NoneType)
     in
     let t1 = typechecker e1 env and
         t2 = typechecker e2 env
     in
     aux t1 t2
  | And (e1, e2, _) | Or (e1, e2, _) ->
     let t1 = typechecker e1 env and
         t2 = typechecker e2 env
     in
     (match (t1, t2) with
      | (BoolType, BoolType) -> BoolType
      | _ -> NoneType)
  | Not (e1,_) -> typechecker e1 env
  | Let (binds, e, _) ->
     let rec add_to_env (bindings : (string * exp * typ option) list) (n_env : typ environment option) =
       match bindings with
       | [] -> ()
       | (id, e1, t1)::rest -> let v = (typechecker e1 (ref n_env)) in
                               (match t1 with
                                | Some t ->
                                   if (t <> v) then
                                     failwith ("for id \"" ^ id ^ "\" types don't match")
                                   else
                                     (Env.bind n_env id v;
                                      add_to_env rest n_env)

                                | None ->
                                   Env.bind n_env id v;
                                   add_to_env rest n_env)
     in
     env := Env.begin_scope !env;
     add_to_env binds (!env);
     let res = typechecker e env in
     env := Env.end_scope !env;
     res
  | Fun (arg, e, _) -> env := Env.begin_scope !env;
                       Env.bind !env arg StringType;
                       let t1 = typechecker e env in
                       env := Env.end_scope !env;
                       if t1 = NoneType then t1 else
                       FunType(StringType, t1)
  | App (e1, e2, _) -> let t1 = typechecker e1 env in
                        match t1 with
                        | FunType (t0, tr) -> let t2 = typechecker e2 env in
                                              if t2 != t0 then NoneType else
                                                tr
                        | _ -> NoneType
  | New(e,_) ->  RefType(typechecker e env)
  | Deref(e,_) ->
     (match typechecker e env with
      | RefType r -> r
      | _ -> NoneType)
  | Assign(id,e,_) -> if (Env.find !env id) = NoneType then NoneType else
                      if (typechecker e env) = NoneType then NoneType else
                        UnitType
  | While (e1,e2,_) -> let t1 =  (typechecker e1 env) and
                         t2 =  (typechecker e2 env) in
                     if t1 != BoolType then NoneType else
                       if t2 = NoneType then NoneType else
                         UnitType
  | IfThenElse (e1,e2,e3,_) -> let t1 = (typechecker e1 env) and
                                 t2 = (typechecker e2 env) and
                                 t3 = (typechecker e3 env)
                             in
                             if t1 != BoolType then NoneType else
                               if t2 = NoneType then NoneType else
                                 if t3 = NoneType then NoneType else
                                   if t2 = t3 then t2 else NoneType
  | IfThen (e1,e2,_) -> let t1 = (typechecker e1 env) and
                          t2 = (typechecker e2 env)
                      in
                      if t1 != BoolType then NoneType else
                        if t2 != UnitType then NoneType else
                          t2
  | Seq(e1,e2,_) -> let e1type = typechecker e1 env in
                  let e2type = typechecker e2 env in
                  if e1type = NoneType then NoneType else
                    if e2type = NoneType then NoneType else
                      e2type
  | PrintLn(e,_) -> if (typechecker e env) = NoneType then NoneType else
                    UnitType
  | Print(e,_) -> if (typechecker e env) = NoneType then NoneType else
                  UnitType
  | UnitExp _-> UnitType
  | String(_,_) -> StringType
