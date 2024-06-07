open Types



let gen_ref t =
  let typeName = string_of_type t in
  let constructor = [
    ".method public <init>()V";
    "aload 0";
    "invokenonvirtual java/lang/Object/<init>()V";
    "return";
    ".end method"
  ] in
  let print = [
    ".method public toString()Ljava/lang/String;";
    ".limit locals 1";
    ".limit stack 10";
    "new java/lang/StringBuilder";
    "dup";
    "invokespecial java/lang/StringBuilder/<init>()V";
    "dup";
    "ldc \"ref \"";
    "invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;";
    "dup";
    "aload 0";
    "getfield " ^ typeName ^ "/value " ^ string_of_ref_subtype t;
    (match t with
     | RefType r -> (match r with
         | IntType -> "invokestatic java/lang/Integer/toString(I)Ljava/lang/String;"
         | BoolType -> "invokestatic java/lang/Boolean/toString(Z)Ljava/lang/String;"
         | FloatType -> "invokestatic java/lang/Float/toString(F)Ljava/lang/String;"
         | RefType _ -> "invokevirtual java/lang/Object/toString()Ljava/lang/String;"
         | StringType | UnitType -> ""
         | FunType _ -> failwith "FunType in gen_ref not implemented"
         | NoneType -> failwith "NoneType in gen_ref")
     | _ -> failwith "Not a RefType in gen_ref"
    );
      "invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;";
      "dup";
    "invokevirtual java/lang/StringBuilder/toString()Ljava/lang/String;";
    "areturn";
    ".end method"
  ] in
  let f = (".class public " ^ typeName)  ::
          (".super java/lang/Object") ::
          (".field public value " ^ string_of_ref_subtype t) ::
          constructor @ print
  and oc = open_out (typeName ^".j") in
  List.iter (fun x -> Printf.fprintf oc "%s\n" x) f; close_out oc;
  typeName
