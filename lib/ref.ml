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
    "getstatic java/lang/System/out Ljava/io/PrintStream;";
    "aload 0";
    "getfield " ^ typeName ^ "/value " ^ string_of_ref_subtype t;]
    @
    (match t with
     | Types.FunType _ -> []
     | _ -> ["ldc \"ref_" ^ string_of_type t ^ "\"" ]  )

    @
    [
      "return";
      ".end method"
    ] in
  let f = (".class public " ^ typeName)  ::
          (".super java/lang/Object") ::
          (".field public value " ^ string_of_ref_subtype t) ::
          constructor @ print
  and oc = open_out (typeName ^".j") in
  List.iter (fun x -> Printf.fprintf oc "%s\n" x) f; close_out oc;
  typeName
