(* let counter_closures = ref 0

let gen_number_closure () =
  let n = !counter_closures in
  counter_closures := n + 1;
  n

let gen_closure args env clsr_t frame_n=
  let rec gen_fields n bindings =
    match bindings with
    | [] -> []
    | (_,t)::rest ->
       let field = Printf.sprintf ".field public loc_%d %s" n (Frame.type_to_string t) in
       field :: gen_fields (n+1) rest
  in
  let field = gen_fields 0 args in
  let constructor = [
      ".method public <init>()V";
      "aload 0";
      "invokenonvirtual java/lang/Object/<init>()V";
      "return";
      ".end method"
    ] in
    let clsr = gen_number_closure () in
  let class_name = Printf.sprintf "Closure_%d" clsr in
  let apply =
    (".method public apply(" ^ sprint ^ ")" ^ (Frame.type_to_string (clsr_t))) ::
    (* TODO codigo aqui*)
                               (* @ [ "return"; *)
                               [".end method"]
  in
    let c = [
      ".class public " ^ class_name;
      ".super java/lang/Object";
      ".field public SL " ^ if env = None
                           then "Ljava/lang/Object;"
                           else "Lframe_" ^ string_of_int frame_n ^ ";"; (* TODO o frame_n pode estar errado *)
    ] @ field @ constructor @ apply in
  let oc = open_out (class_name ^ ".j") in
  List.iter (fun x -> Printf.fprintf oc "%s\n" x) c;
  close_out oc;
  clsr *)
