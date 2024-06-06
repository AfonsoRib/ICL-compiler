open Env
open Types

type exp =
  |Add of exp * exp * typ
  |Sub of exp * exp * typ
  |Mult of exp * exp * typ
  |Div of exp * exp * typ
  |Addf of exp * exp * typ
  |Subf of exp * exp * typ
  |Multf of exp * exp * typ
  |Divf of exp * exp * typ
  |Fact of int * typ
  |FloatFact of float * typ
  |Eq of exp * exp * typ
  |Ne of exp * exp * typ
  |Le of exp * exp * typ
  |Ge of exp * exp * typ
  |Lt of exp * exp * typ
  |Gt of exp * exp * typ
  |And of exp * exp * typ
  |Or of exp * exp * typ
  |Not of exp * typ
  |Statement of bool * typ
  |Let of (string * exp * typ) list * exp * typ
  |Id of string * typ
  |New of exp * typ
  |Deref of exp * typ
  |Assign of exp * exp * typ  
  |While of exp * exp * typ
  |IfThenElse of exp * exp * exp * typ
  |IfThen of exp * exp * typ
  |PrintLn of exp * typ
  |Print of exp * typ
  |Seq of exp * exp * typ
  |UnitExp of typ
  |String of string * typ
  |Fun of (string * typ) list * exp * typ
  |App of exp * exp list * typ


type eval_result =
  | Int of int
  | Float of float
  | Bool of bool
  | Ref of eval_result ref
  | Str of string
  | Closure of (string * typ) list * exp * eval_result environment option ref
  | Unit

let rec string_of_eval_result = function
  | Int n -> "Int " ^ string_of_int n
  | Float f -> "Float " ^ string_of_float f
  | Bool b -> "Bool" ^ string_of_bool b
  | Unit -> "Unit"
  | Ref r ->  (string_of_eval_result !r ) ^ " ref"
  | Str s -> "String " ^ s
  | Closure _ -> "Closure placeholder"

let rec string_of_ref t contents_start contents_end typ =
  match t with
  | Ref r -> string_of_ref !r (contents_start ^ "{contents = ") ("}" ^ contents_end ) (typ ^ " ref" )
  | Bool b -> "bool" ^ typ ^ " = " ^ contents_start ^ string_of_bool b ^ contents_end
  | Unit -> "unit" ^ typ ^ " = " ^ contents_start ^ "()" ^ contents_end
  | Int n -> "int" ^ typ ^ " = " ^ contents_start ^ string_of_int n ^ contents_end
  | Float f -> "float" ^ typ ^ " = " ^ contents_start ^ string_of_float f ^ contents_end
  | Str s -> "string" ^ typ ^ " = " ^ contents_start ^ s ^ contents_end
  | Closure _ -> "Closure placeholder"

let rec string_of_eval_result_clean = function
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Unit -> "Unit"
  | Ref r -> string_of_eval_result_clean !r
  | Str s -> s
  | Closure _ -> "Closure placeholder"

let rec eval (expr : exp) (env : eval_result environment option ref) : eval_result =
  let not_operation f x =
    match x with
    | Bool b -> f b
    | _ -> failwith "Can only apply not to boolean"
  in
  let boolean_operation f x y =
    match (x,y) with
    | (Bool b1, Bool b2) -> f b1 b2
    | _ -> failwith "Can only apply boolean operation to booleans"
  in
  let rec inequality_operation f x y =
    match (x, y) with
    | (Int i1, Int i2) -> f (Int i1) (Int i2)
    | (Float f1, Float f2) -> f (Float f1) (Float f2)
    | (Bool b1, Bool b2) -> f (Bool b1) (Bool b2)
    | (Str s1, Str s2) -> f (Str s1) (Str s2)
    | (Ref r1, Ref r2) -> inequality_operation f !r1 !r2
    | _ -> failwith "Type mismatch"
  in
  let int_operation fint x y =
    match (x, y) with
    | (Int i1, Int i2) -> Int (fint i1 i2)
    | _ -> failwith "Type mismatch"
  in
  let float_operation ffloat x y =
    match (x, y) with
    | (Float f1, Float f2) -> Float (ffloat f1 f2)
    | _ -> failwith "Type mismatch"
  in
  match expr with
  | Fact (n, _) ->  Int n
  | FloatFact (f, _) -> Float f
  | Statement (b, _) -> Bool b
  | Id (x, _) -> Env.find !env x
  | Add (e1, e2, _) ->  (int_operation (+) (eval e1 env) (eval e2 env))
  | Mult (e1, e2, _) -> (int_operation ( * ) (eval e1 env) (eval e2 env))
  | Sub (e1, e2, _) -> (int_operation (-) (eval e1 env) (eval e2 env))
  | Div (e1, e2, _) -> (int_operation (/) (eval e1 env) (eval e2 env))
  | Addf (e1, e2, _) ->  (float_operation (+.) (eval e1 env) (eval e2 env))
  | Multf (e1, e2, _) -> (float_operation ( *. ) (eval e1 env) (eval e2 env))
  | Subf (e1, e2, _) -> (float_operation (-.) (eval e1 env) (eval e2 env))
  | Divf (e1, e2, _) -> (float_operation ( /. ) (eval e1 env) (eval e2 env))
  | Eq (e1, e2, _) -> Bool(inequality_operation (=) (eval e1 env) (eval e2 env))
  | Ne (e1, e2, _) -> Bool(inequality_operation (<>) (eval e1 env) (eval e2 env))
  | Le (e1, e2, _) -> Bool(inequality_operation (<=) (eval e1 env) (eval e2 env))
  | Ge (e1, e2, _) -> Bool(inequality_operation (>=) (eval e1 env) (eval e2 env))
  | Lt (e1, e2, _) -> Bool(inequality_operation (<) (eval e1 env) (eval e2 env))
  | Gt (e1, e2, _) -> Bool(inequality_operation (>) (eval e1 env) (eval e2 env))
  | And (e1,e2,_) -> let ex = eval e1 env in
    if ex = Bool false then ex else Bool(boolean_operation (&&) ex (eval e2 env))
  | Or (e1,e2,_) -> let ex = eval e1 env in
    if ex = Bool true then ex else Bool(boolean_operation (||) ex (eval e2 env))
  | Not (e1, _) -> Bool(not_operation (not) (eval e1 env))
  | Let (binds, e, _) ->
    let rec add_to_env (bindings : (string * exp * typ) list) (n_env : eval_result environment option) =
      match bindings with
      | [] -> ()
      | (id, e1, _)::rest -> let v = (eval e1 (ref n_env)) in
        Env.bind n_env id v;
        add_to_env rest n_env
    in
    env := Env.begin_scope !env;
    add_to_env binds (!env);
    let res = eval e env in
    env := Env.end_scope !env;
    res
  | New(e, _) -> Ref(ref (eval e env))
  | Deref(e, _) -> (let result = (eval e env) in match result with Ref r -> !r | _ -> failwith "Not a reference")
  | Assign(x, e, _) ->
    let
      (* ref_result = Env.find !env x and *)
      ref_result = eval x env and
    value_to_assign = eval e env in
    (match ref_result with
     | Ref r -> r := value_to_assign
     | _ -> failwith "Left-hand side of assignment must be a reference")
  ; Unit
  | While(e1,e2, _) -> while (match (eval e1 env) with Bool b -> b | _ -> failwith "not a boolean") do
      ignore(eval e2 env)
    done;
    Unit
  | PrintLn(e, _) -> print_endline (string_of_eval_result_clean (eval e env));
    Unit
  | Print(e, _) -> print_string (string_of_eval_result_clean (eval e env));
    Unit
  | Seq(e1,e2, _) -> ignore(eval e1 env); eval e2 env
  | IfThenElse(e1,e2,e3, _) -> (match (eval e1 env) with
      | Bool b -> if b then eval e2 env else eval e3 env
      | _ -> failwith "not a boolean")
  | IfThen(e1,e2, _ ) -> (match (eval e1 env) with
      | Bool b -> if b then eval e2 env else Unit
      | _ -> failwith "not a boolean")
  | UnitExp _ -> Unit
  | String(s, _) -> Str s
  | Fun(args, e1, _ ) ->
    Closure(args, e1, env)
  | App (e1,args,_) ->
    let clsr = eval e1 env in
    match clsr with
    | Closure (clsr_args, clsr_body, clsr_env) ->
      let rec add_to_env (bindings : (string*typ) list) (vals : exp list) (n_env : eval_result environment option) =
        match bindings, vals with
        | [], [] -> ()
        | (bind, _)::binds, e::es -> let v = (eval e (ref n_env)) in
          Env.bind n_env bind v;
          add_to_env binds es n_env
        | _ -> failwith "size mismatch"
      in
      clsr_env := Env.begin_scope !env;
      add_to_env clsr_args args !clsr_env;
      let res = eval clsr_body clsr_env in
      clsr_env := Env.end_scope !clsr_env;
      res
    | _ -> failwith "not a closure"
