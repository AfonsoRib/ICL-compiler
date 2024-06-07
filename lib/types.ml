type typ=
  | IntType
  | FloatType
  | BoolType
  | UnitType
  | RefType of typ
  | NoneType
  | StringType
  | FunType of typ list * typ

let rec string_of_type t =
  match t with 
  | IntType  -> "int"
  | FloatType -> "float"
  | BoolType -> "bool"
  | UnitType -> "unit"
  | RefType r -> "ref_" ^ string_of_type r
  | NoneType -> "none"
  | StringType -> "string"
  | FunType (args, ret) -> 
    Printf.sprintf "__fun_%s_%s__" (String.concat "_" (List.map string_of_type args)) (string_of_type ret)

let jvmTypeString_of_type t=
  match t with
  | IntType -> "I"
  | FloatType -> "F"
  | BoolType -> "Z"
  | RefType _ -> "L"^ string_of_type t ^ ";"
  | NoneType -> "none"
  | UnitType | StringType -> "Ljava/lang/String;"
  | FunType (args,ret) -> "Lclosure_interface_" ^ String.concat "_" (List.map string_of_type args) ^ "_" ^ string_of_type ret ^ ";"
(* | _ -> failwith "not supported" *)

let rec natural_string_of_type t =
  match t with
  | IntType -> "int"
  | FloatType -> "float"
  | BoolType -> "bool"
  | UnitType -> "unit"
  | RefType r -> "ref " ^ natural_string_of_type r
  | NoneType -> "none"
  | StringType -> "string"
  | FunType (args, ret) -> 
    Printf.sprintf "(%s) -> %s" (String.concat ", " (List.map string_of_type args)) (string_of_type ret)


let string_of_ref_subtype t =
  let aux t1 =
    match t1 with
    | RefType r -> "Lref_" ^ string_of_type r ^ ";"
    | IntType  -> "I"
    | FloatType -> "F"
    | BoolType -> "Z"
    | StringType -> "Ljava/lang/String;"
    | UnitType -> "Ljava/lang/String;"
    | _ -> "none"
  in
  match t with
  | RefType r -> aux r
  | _ -> failwith (string_of_type t ^ " not a ref statement")