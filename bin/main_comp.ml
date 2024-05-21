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


let main =
  let filename = "test.txt" in (* Specify your input file name here *)
  let channel = open_in filename in
  let outputname = "jasmin.j" in
  let outchannel = open_out outputname in
  try
    let lexbuf = Lexing.from_channel channel in
    let ast = Parser.start Lexer.token lexbuf in
    let typecheck = (Typechecker.typechecker ast (ref None)) in
    let typ = fst typecheck in
    let ast2 = snd typecheck in
    if typ = Types.NoneType then failwith "Failed typechecker pass. Expression type: None";
    (* print_endline "ref" (Ref.string_of_type typ); *)
    let res = Comp.comp ast2 (ref None)in
    output_string outchannel preamble;
    List.iter (fun x -> output_string outchannel ((Comp.jvmString x) ^ "\n")) res;
    output_string outchannel footer
  with | Failure msg -> print_endline msg
       | _ -> print_endline "Syntax error!"

let () =
  main
