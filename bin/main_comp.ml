open Icl

let footer = "
\treturn
.end method
"

let printType t =
  "\t" ^ (match t with
  | Types.IntType -> "invokestatic java/lang/String/valueOf(I)Ljava/lang/String;"
  | Types.FloatType ->"invokestatic java/lang/String/valueOf(D)Ljava/lang/String;"
  | Types.UnitType | Types.StringType -> "invokestatic java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;"
  | Types.BoolType -> "invokestatic java/lang/String/valueOf(Z)Ljava/lang/String;"
  | _ -> "; not implemented")
  ^ "\n\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"

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
\t; setup local variables:
\t;    1 - the PrintStream object held in java.lang.out
\tgetstatic java/lang/System/out Ljava/io/PrintStream;
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
    let ast = snd typecheck in
    if typ = Types.NoneType then failwith "Failed typechecker pass. Expression type: None";
    let res = Comp.comp ast in
    output_string outchannel preamble;
    List.iter (fun x -> output_string outchannel ((Comp.jvmString x) ^ "\n")) res;
    if typ != Types.UnitType then
      output_string outchannel (printType typ);
    output_string outchannel footer
  with | Failure msg -> print_endline msg
       | _ -> print_endline "Syntax error!"

let () =
  main
