open Types
let counter_frames = ref 0

let gen_number_frame () =
  let fresh_number = !counter_frames in
  counter_frames := fresh_number + 1;
  fresh_number

let type_to_string t=
  match t with
  | IntType -> "I"
  | FloatType -> "D"
  | BoolType -> "Z"
  (* | _ -> failwith "not supported" *)
  | UnitType -> "unit"
  | RefType _ -> "ref"
  | NoneType -> "none"
  | StringType -> "str"


let gen_frame bindings env =
  let rec gen_fields n bindings =
    match bindings with
    | [] -> []
    | (_,_,t)::rest ->
       let field = Printf.sprintf ".field public loc_%d %s" n (type_to_string t) in
       field :: gen_fields (n+1) rest
  in
  let fields = gen_fields 0 bindings in
  let constructor = [
      ".method public <init>()V";
      "aload 0";
      "invokenonvirtual java/lang/Object/<init>()V";
      "return";
      ".end method"
    ] in
  let fn = gen_number_frame () in
  let frame_number = string_of_int (fn)  in 
  let f = (".class public frame_" ^ frame_number)  ::
            ".super java/lang/Object" ::
              (".field public SL " ^ if env = None
                                   then "Ljava/lang/Object;"
                                   else "Lframe_" ^ string_of_int((!counter_frames) - 2) ^ ";")
 ::
                fields @ constructor
  and oc = open_out ("frame_"^ frame_number ^".j") in
  List.iter (fun x -> Printf.fprintf oc "%s\n" x) f; close_out oc;
  fn

