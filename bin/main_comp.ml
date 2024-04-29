open Icl

let footer = "
return
.end method
"

let printType t =
  (match t with
  | Typechecker.IntType -> "invokestatic java/lang/String/valueOf(I)Ljava/lang/String;"
  | Typechecker.FloatType ->"invokestatic java/lang/String/valueOf(D)Ljava/lang/String;"
  | Typechecker.UnitType | Typechecker.StringType -> "invokestatic java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;"
  | Typechecker.BoolType -> "invokestatic java/lang/String/valueOf(Z)Ljava/lang/String;"
  | _ -> "; not implemented")
  ^ "\ninvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V"

let preamble =
".class public Demo
.super java/lang/Object
.method public <init>()V
   aload_0
   invokenonvirtual java/lang/Object/<init>()V
   return
.end method
.method public static main([Ljava/lang/String;)V
 .limit locals 10
 .limit stack 256
 ; setup local variables:
 ;    1 - the PrintStream object held in java.lang.out
getstatic java/lang/System/out Ljava/io/PrintStream;
"


let main =
  let filename = "test.txt" in (* Specify your input file name here *)
  let channel = open_in filename in
  let outputname = "jasmin.j" in
  let outchannel = open_out outputname in
  try
    let lexbuf = Lexing.from_channel channel in
    let ast = Parser.start Lexer.token lexbuf in
    let typecheck = Typechecker.typechecker ast (ref None) in
    if typecheck = Typechecker.NoneType then failwith "Failed typechecker pass. Expression type: None";
    let res = Comp.comp ast in
    output_string outchannel preamble;
    List.iter (fun x -> output_string outchannel ((Comp.jvmString x) ^ "\n")) res;
    if typecheck != Typechecker.UnitType then
      output_string outchannel (printType typecheck);
    output_string outchannel footer
  with | Failure msg -> print_endline msg
       | _ -> print_endline "Syntax error!"

let () =
  main
