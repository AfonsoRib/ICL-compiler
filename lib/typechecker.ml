open Ast
open Env

type typ=
  | IntType
  | BoolType
  | UnitType
  | RefType of typ
  | NoneType
  | StringType

let rec typ_str typ_string =
  let typ_list = List.rev (String.split_on_char ' ' typ_string) in
  match typ_list with
  | [] -> failwith "no such type"
  | [t] -> (match t with
          | "int" -> IntType
          | "bool" -> BoolType
          | "unit" -> UnitType
          | "string" -> StringType
          | _  -> failwith "no such type ")
  | t::ts -> (match t with
            | "ref" -> RefType(typ_str (String.concat " " (List.rev ts)))
            | _ -> failwith "no such type")

let rec str_typ = function
  | IntType -> "Int"
  | BoolType -> "Bool"
  | UnitType -> "unit"
  | RefType t -> "Ref " ^ (str_typ t)
  | NoneType -> "None"
  | StringType -> "String"

let rec typechecker (e : Ast.exp) (env : typ environment option ref): typ =
  let typechecker_aux e1 e2 t_in t_out =
    let resolve_ref t =
      match t with
      | RefType inner -> inner
      | _ -> t
    in
    let t1 = resolve_ref (typechecker e1 env) in
    let t2 = resolve_ref (typechecker e2 env) in
    if t1 = t_in && t2 = t_in then t_out else NoneType
  in
  match e with
  | Fact _ ->  IntType
  | Statement _ -> BoolType
  | Id x -> Env.find !env x
  | Add (e1, e2) ->  typechecker_aux e1 e2 IntType IntType
  | Mult (e1, e2) -> typechecker_aux e1 e2 IntType IntType
  | Sub (e1, e2) -> typechecker_aux e1 e2 IntType IntType
  | Div (e1, e2) -> typechecker_aux e1 e2 IntType IntType
  | Eq (e1, e2) -> typechecker_aux e1 e2 IntType BoolType
  | Ne (e1, e2) -> typechecker_aux e1 e2 IntType BoolType
  | Le (e1, e2) -> typechecker_aux e1 e2 IntType BoolType
  | Ge (e1, e2) -> typechecker_aux e1 e2 IntType BoolType
  | Lt (e1, e2) -> typechecker_aux e1 e2 IntType BoolType
  | Gt (e1, e2) -> typechecker_aux e1 e2 IntType BoolType
  | And (e1, e2) -> typechecker_aux e1 e2 BoolType BoolType
  | Or (e1, e2) -> typechecker_aux e1 e2 BoolType BoolType
  | Not (e1) -> typechecker e1 env
  | Let (binds, e) ->
     let rec add_to_env (bindings : (string * exp * string option) list) (n_env : typ environment option) =
       match bindings with
       | [] -> ()
       | (id, e1, t1)::rest -> let v = (typechecker e1 (ref n_env)) in
                               (match t1 with
                                | Some t ->
                                  let typ = typ_str t in
                                   if (typ <> v) then
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
  (* | _ -> NoneType *)
  | New(e) ->  RefType(typechecker e env)
  | Deref(e) -> typechecker e env
  | Assign(id,e) -> if (Env.find !env id) = NoneType then NoneType else
                      if (typechecker e env) = NoneType then NoneType else
                        UnitType
  | While (e1,e2) -> let t1 =  (typechecker e1 env) and
                         t2 =  (typechecker e2 env) in
                     if t1 != BoolType then NoneType else
                       if t2 = NoneType then NoneType else
                         UnitType
  | IfThenElse (e1,e2,e3) -> let t1 = (typechecker e1 env) and
                                 t2 = (typechecker e2 env) and
                                 t3 = (typechecker e3 env)
                             in
                             if t1 != BoolType then NoneType else
                               if t2 = NoneType then NoneType else
                                 if t3 = NoneType then NoneType else
                                   if t2 = t3 then t2 else NoneType
  | IfThen (e1,e2) -> let t1 = (typechecker e1 env) and
                          t2 = (typechecker e2 env)
                      in
                      if t1 != BoolType then NoneType else
                        if t2 != UnitType then NoneType else
                          t2
  | Seq(e1,e2) -> let e1type = typechecker e1 env in
                  let e2type = typechecker e2 env in
                  if e1type = NoneType then NoneType else
                    if e2type = NoneType then NoneType else
                      e2type
  | PrintLn(e) -> if (typechecker e env) = NoneType then NoneType else
                    UnitType
  | Print(e) -> if (typechecker e env) = NoneType then NoneType else
                  UnitType
  | UnitExp -> UnitType
