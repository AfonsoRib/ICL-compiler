open Icl

let footer = "
\treturn
.end method
"

let preamble =
".class public Demo
.super java/lang/Object
.method public <init>()V
\taload_0
\tinvokenonvirtual java/lang/Object/<init>()V
\treturn
.end method
.method public static main([Ljava/lang/String;)V
\t.limit locals 10
\t.limit stack 256
"

(* let print_position outx lexbuf = *)
(*   let pos = lexbuf.lex_curr_p in *)
(*   Printf.fprintf outx "File \"%s\", line %d, character %d:\n" *)
(*     pos.pos_fname *)
(*     pos.pos_lnum *)
(*     (pos.pos_cnum - pos.pos_bol + 1) *)

let main =
  (* let filename = "test.txt" in (\* Specify your input file name here *\) *)
  (* let channel = open_in filename in *)
  let dir_name = "jasmin_bytecode" in
  let permissions = 0o755 in
  (try
     Unix.mkdir dir_name permissions;
     Printf.printf "Directory '%s' created successfully.\n" dir_name;
     Unix.chdir dir_name;
     Printf.printf "Changed directory to '%s'.\n" dir_name
   with
   | Unix.Unix_error (EEXIST, _, _) ->
      Printf.printf "Directory '%s' already exists.\n" dir_name;
      (try
         Unix.chdir dir_name;
        Printf.printf "Changed directory to '%s'.\n" dir_name
       with
       | _ -> failwith "something went wrong creating and cding into the bytecode directory")
   | Unix.Unix_error (e, _, _) ->
      Printf.printf "Failed to create directory '%s': %s\n" dir_name (Unix.error_message e)
   | _ -> failwith "something went wrong creating and cding into the bytecode directory");
  let outputname = "jasmin.j" in
  let outchannel = open_out outputname in
  try
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.start Lexer.token lexbuf in
    let typecheck = (Typechecker.typechecker ast (ref None)) in
    let typ = fst typecheck in
    let ast2 = snd typecheck in
    if typ = Types.NoneType then failwith "Failed typechecker pass. Expression type: None";
    let res = Comp.comp ast2 (ref None)in
    output_string outchannel preamble;
    List.iter (fun x -> output_string outchannel ((Comp.jvmString x) ^ "\n")) res;
    output_string outchannel footer
  with | Failure msg -> print_endline msg
       (* | Parser.Error -> *)
       (*    print_position stderr lexbuf; *)
       (*    prerr_endline "Syntax error"; *)
       | _ -> print_endline "Syntax error!"

let () =
  main
