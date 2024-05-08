open Icl
open Printf

let main =
  while true do
    try
      let lexbuf = Lexing.from_channel stdin in
      let ast = Parser.start Lexer.token lexbuf in
      let typecheck = Typechecker.typechecker ast (ref None) in
      if typecheck = Types.NoneType then failwith "Failed typechecker pass. Expression type: None";
      (* printf "type: %s\n" (Typechecker.str_typ typecheck); *)
      let res = Ast.eval ast (ref None) in
      match res with
      | Int n -> printf "- : int = %d\n" (n); flush stdout
      | Float f -> printf "- : float = %f\n" (f); flush stdout
      | Bool b -> printf "- : bool = %B\n" (b); flush stdout
      | Ref _ -> printf "- : %s\n" (Ast.string_of_ref res "" "" ""); flush stdout
      | Unit -> print_endline "- : unit = ()"; flush stdout
      | Str s -> printf "- : string = %s\n" s; flush stdout
    with | Failure msg -> print_endline msg
         |_-> print_endline "Syntax error!"
  done

let () =
  main
