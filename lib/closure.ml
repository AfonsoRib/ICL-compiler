let counter_closures = ref 0

let gen_number_closure () =
  let n = !counter_closures in
  counter_closures := n + 1;
  n

let create_interface args ret name =
  let fields = List.map (fun t -> Frame.type_to_string t) args in
  let file = [
    ".class public " ^ name;
    ".super java/lang/Object";
    ".method public abstract apply(" ^ (String.concat "" fields) ^ ")" ^ (Frame.type_to_string ret);
    ".end method"
  ] in
  let file_str = String.concat "\n" file in
  let file_name = Printf.sprintf "%s.j" name in
  let oc = open_out file_name in
  Printf.fprintf oc "%s" file_str;
  close_out oc;
  ()

let create_closure_file args clsrt_t (cur_frame : int Frame.frame_env option) compiled_body=
  let interface_name = "closure_interface_" ^
                       String.concat "_" (List.map Ref.string_of_type args)
                       ^ "_" ^ Ref.string_of_type clsrt_t in
  create_interface args clsrt_t interface_name;
  let prev_frame = (Option.get cur_frame).prev in
  let prev_frame_str = match prev_frame with
    | None -> "Ljava/lang/Object;"
    | Some f -> "Lframe_" ^ string_of_int f.id ^ ";" in
  let cur_frame_str = "Lframe_" ^ string_of_int (Option.get cur_frame).id ^ ";" in
  let fields = List.map (fun t -> Frame.type_to_string t) args in
  let clsr_n = gen_number_closure () in
  let class_name = Printf.sprintf "Closure_%d" clsr_n in
  let preamble= [
    ".class public " ^ class_name;
    ".super java/lang/Object";
    ".implements " ^ interface_name;
    if prev_frame = None
    then ".field public SL Ljava/lang/Object;"
    else ".field public SL Lframe_" ^ string_of_int (Option.get prev_frame).id ^ ";"
  ] in
  let constructor = [
    ".method public <init>()V";
    "aload 0";
    "invokenonvirtual java/lang/Object/<init>()V";
    "return";
    ".end method"
  ] in
  let comp_fields_intermediate = (List.mapi 
                                    (fun i fields -> [ "dup";
                                                       "iload " ^ (string_of_int (i+1)); (*TODO mudar load em função do tipo*)
                                                       "putfield frame_" ^ (string_of_int (Option.get cur_frame).id) ^ "/loc_" ^ (string_of_int i) ^ " " ^ fields
                                                     ]) fields ) in
  let comp_fields  = List.flatten comp_fields_intermediate
  in
  let apply = [
    ".method public apply(" ^ (String.concat "" fields) ^ ")" ^ (Frame.type_to_string clsrt_t);
    ".limit locals " ^ (string_of_int ((List.length args) + 2));
    "new frame_" ^ (string_of_int (Option.get cur_frame).id);
    "dup";
    "invokespecial frame_" ^ (string_of_int (Option.get cur_frame).id) ^ "/<init>()V";
    "dup";
    "aload 0";
    "getfield " ^ class_name ^ "/SL " ^ prev_frame_str;
    "putfield frame_" ^ (string_of_int (Option.get cur_frame).id) ^ "/SL " ^ cur_frame_str;

  ] @ 
    comp_fields
    @ ["astore " ^  string_of_int ((List.length comp_fields_intermediate)+1)]
    @ compiled_body 
    @ [
      (match clsrt_t with
       | Types.UnitType -> "return"
       | Types.IntType -> "ireturn"
       | Types.FloatType -> "freturn"
       | Types.BoolType -> "ireturn"
       | Types.StringType -> "areturn"
       | Types.RefType _ -> "areturn"
       | Types.FunType _ -> "areturn"
       | Types.NoneType -> failwith "Cannot return NoneType");
      ".end method"
    ]
  in
  let file = List.flatten [preamble; constructor; apply] in
  let file_str = String.concat "\n" file in
  let file_name = Printf.sprintf "Closure_%d.j" clsr_n in
  let oc = open_out file_name in
  Printf.fprintf oc "%s" file_str;
  close_out oc;
  class_name

