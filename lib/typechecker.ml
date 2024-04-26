open Ast
open Env

type typ=
  | IntType
  | FloatType
  | NumType
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
          | "float" -> FloatType
          | "bool" -> BoolType
          | "unit" -> UnitType
          | "string" -> StringType
          | _  -> failwith "no such type ")
  | t::ts -> (match t with
            | "ref" -> RefType(typ_str (String.concat " " (List.rev ts)))
            | _ -> failwith "no such type")

let rec str_typ = function
  | IntType -> "Int"
  | FloatType -> "Float"
  | NumType -> "Num"
  | BoolType -> "Bool"
  | UnitType -> "unit"
  | RefType t -> "Ref " ^ (str_typ t)
  | NoneType -> "None"
  | StringType -> "String"

let rec typechecker (e : Ast.exp) (env : typ environment option ref): typ =
  let typechecker_aux e1 e2 t_in t_out =
    let resolve_num t1 t2 =
      if t1 = IntType && t2 = IntType then IntType
      else if t1 = FloatType && t2 = FloatType then FloatType
      else if t1 = IntType && t2 = FloatType then FloatType
      else if t1 = FloatType && t2 = IntType then FloatType
      else NoneType
    in
    let t1 =  (typechecker e1 env) in
    let t2 =  (typechecker e2 env) in

    if t_in = NumType && t_out = NumType then
      resolve_num t1 t2

    else if t_in = NumType && t_out = BoolType then
      if (t1 = IntType && t2 = IntType) || (t1 = FloatType && t2 = FloatType) then BoolType else NoneType

    else if t_in = NumType && t_out != NumType then
        if resolve_num t1 t2 != NoneType then t_out else NoneType

    else if t1 = t_in && t2 = t_in then
        t_out else NoneType
  in
  let rec typechecker_aux_aux e1 e2 (t_ins : typ list) t_out =
    match t_ins with
    | t::ts -> if typechecker_aux e1 e2 t t_out = t_out || typechecker_aux_aux e1 e2 ts t_out = t_out then t_out else NoneType
    | [] ->  NoneType
  in
  match e with
  | Fact _ ->  IntType
  | FloatFact _ -> FloatType
  | Statement _ -> BoolType
  | Id x -> Env.find !env x
  | Add (e1, e2) | Mult (e1, e2) | Sub (e1, e2) | Div (e1, e2) -> typechecker_aux e1 e2 NumType NumType
  | Ne (e1, e2)| Le (e1, e2)| Ge (e1, e2)| Lt (e1, e2)| Gt (e1, e2) | Eq (e1, e2) -> typechecker_aux_aux e1 e2 [NumType; BoolType; StringType; (*RefType*)] BoolType
  | And (e1, e2) | Or (e1, e2) -> typechecker_aux e1 e2 BoolType BoolType
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
  | Deref(e) ->
     (match typechecker e env with
      | RefType r -> r
      | _ -> NoneType)
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
  | String(_) -> StringType
