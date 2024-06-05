open Types

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
    Printf.sprintf "(%s) -> %s" (String.concat ", " (List.map string_of_type args)) (string_of_type ret)

  let string_of_ref_subtype t =
  let aux t1 =
    match t1 with
      | RefType r -> "Lref_" ^ string_of_type r ^ ";"
      | IntType  -> "I"
      | FloatType -> "F"
      | BoolType -> "Z"
      | _ -> "none"
  in
  match t with
  | RefType r -> aux r
  | _ -> failwith (string_of_type t ^ " not a ref statement")

let gen_ref t =
  let typeName = string_of_type t in
    let constructor = [
      ".method public <init>()V";
      "aload 0";
      "invokenonvirtual java/lang/Object/<init>()V";
      "return";
      ".end method"
    ] in
    let f = (".class public " ^ typeName)  ::
            (".super java/lang/Object") ::
              (".field public value " ^ string_of_ref_subtype t) ::
                                         constructor
  and oc = open_out (typeName ^".j") in
  List.iter (fun x -> Printf.fprintf oc "%s\n" x) f; close_out oc;
  typeName
