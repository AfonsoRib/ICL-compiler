open Env

type exp =
  |Add of exp * exp
  |Sub of exp * exp
  |Mult of exp * exp
  |Div of exp * exp
  |Fact of int
  |FloatFact of float
  |Eq of exp * exp
  |Ne of exp * exp
  |Le of exp * exp
  |Ge of exp * exp
  |Lt of exp * exp
  |Gt of exp * exp
  |And of exp * exp
  |Or of exp * exp
  |Not of exp
  |Statement of bool
  |Let of (string * exp * string option) list * exp
  |Id of string
  |New of exp
  |Deref of exp
  |Assign of string * exp
  |While of exp * exp
  |IfThenElse of exp * exp * exp
  |IfThen of exp * exp
  |PrintLn of exp
  |Print of exp
  |Seq of exp * exp
  |UnitExp
  |String of string

type eval_result =
  | Int of int
  | Float of float
  | Bool of bool
  | Ref of eval_result ref
  | Str of string
  | Unit

let rec string_of_eval_result = function
  | Int n -> "Int " ^ string_of_int n
  | Float f -> "Float " ^ string_of_float f
  | Bool b -> "Bool" ^ string_of_bool b
  | Unit -> "Unit"
  | Ref r ->  (string_of_eval_result !r ) ^ " ref"
  | Str s -> "String " ^ s

let rec string_of_ref t contents_start contents_end typ =
  match t with
  | Ref r -> string_of_ref !r (contents_start ^ "{contents = ") ("}" ^ contents_end ) (typ ^ " ref" )
  | Bool b -> "bool" ^ typ ^ " = " ^ contents_start ^ string_of_bool b ^ contents_end
  | Unit -> "unit" ^ typ ^ " = " ^ contents_start ^ "()" ^ contents_end
  | Int n -> "int" ^ typ ^ " = " ^ contents_start ^ string_of_int n ^ contents_end
  | Float f -> "float" ^ typ ^ " = " ^ contents_start ^ string_of_float f ^ contents_end
  | Str s -> "string" ^ typ ^ " = " ^ contents_start ^ s ^ contents_end

let rec string_of_eval_result_clean = function
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Unit -> "Unit"
  | Ref r -> string_of_eval_result_clean !r
  | Str s -> s

let rec eval (expr : exp) (env : eval_result environment option ref) : eval_result =
  let not_operation f x =
    match x with
    | Bool b -> f b
    | _ -> failwith "Can only apply not to boolean"
  in
  let boolean_operation f x y =
    (*
    let ex = eval x env in
      match (ex, f) with
      | (Bool false, (&&)) -> f ex ex
      | (Bool true, (||)) -> f ex ex
      | (Bool b1, _) ->
        match (eval y env) with
        | Bool b2 -> f b1 b2)
        *)
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
  let arythmetic_operation fint ffloat x y =
    match (x, y) with
    | (Int i1, Int i2) -> Int (fint i1 i2)
    | (Float f1, Float f2) -> Float (ffloat f1 f2)
    | (Int i1, Float f2) -> Float (ffloat (float_of_int i1) f2)
    | (Float f1, Int i2) -> Float (ffloat f1 (float_of_int i2))
    | _ -> failwith "Type mismatch"
  in
  match expr with
  | Fact n ->  Int n
  | FloatFact f -> Float f
  | Statement b -> Bool b
  | Id x -> Env.find !env x
  | Add (e1, e2) ->  (arythmetic_operation (+) (+.) (eval e1 env) (eval e2 env))
  | Mult (e1, e2) -> (arythmetic_operation ( * ) ( *. ) (eval e1 env) (eval e2 env))
  | Sub (e1, e2) -> (arythmetic_operation (-) (-.) (eval e1 env) (eval e2 env))
  | Div (e1, e2) -> (arythmetic_operation (/) ( /. ) (eval e1 env) (eval e2 env))
  | Eq (e1, e2) -> Bool(inequality_operation (=) (eval e1 env) (eval e2 env))
  | Ne (e1, e2) -> Bool(inequality_operation (<>) (eval e1 env) (eval e2 env))
  | Le (e1, e2) -> Bool(inequality_operation (<=) (eval e1 env) (eval e2 env))
  | Ge (e1, e2) -> Bool(inequality_operation (>=) (eval e1 env) (eval e2 env))
  | Lt (e1, e2) -> Bool(inequality_operation (<) (eval e1 env) (eval e2 env))
  | Gt (e1, e2) -> Bool(inequality_operation (>) (eval e1 env) (eval e2 env))
  | And (e1,e2) -> let ex = eval e1 env in
                    if ex = Bool false then ex else Bool(boolean_operation (&&) ex (eval e2 env))
  | Or (e1,e2) -> let ex = eval e1 env in
                  if ex = Bool true then ex else Bool(boolean_operation (&&) ex (eval e2 env))
  | Not (e1) -> Bool(not_operation (not) (eval e1 env))
  | Let (binds, e) ->
     let rec add_to_env (bindings : (string * exp * string option) list) (n_env : eval_result environment option) =
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
  | New(e) -> Ref(ref (eval e env))
  | Deref(e) -> (let result = (eval e env) in match result with Ref r -> !r | _ -> failwith "Not a reference")
  | Assign(x, e) ->
     let ref_result = Env.find !env x and
         value_to_assign = eval e env in
     (match ref_result with
      | Ref r -> r := value_to_assign
      | _ -> failwith "Left-hand side of assignment must be a reference")
     ; Unit
  | While(e1,e2) -> while (match (eval e1 env) with Bool b -> b | _ -> failwith "not a boolean") do
                     ignore(eval e2 env)
                    done;
                    Unit
  | PrintLn(e) -> print_endline (string_of_eval_result_clean (eval e env));
                  Unit
  | Print(e) -> print_string (string_of_eval_result_clean (eval e env));
                Unit
  | Seq(e1,e2) -> ignore(eval e1 env); eval e2 env
  | IfThenElse(e1,e2,e3) -> (match (eval e1 env) with
                  | Bool b -> if b then eval e2 env else eval e3 env
                  | _ -> failwith "not a boolean")
  | IfThen(e1,e2) -> (match (eval e1 env) with
                  | Bool b -> if b then eval e2 env else Unit
                  | _ -> failwith "not a boolean")
  | UnitExp -> Unit
  | String(s) -> Str s
