open OUnit2
open Icl

let eval_string input =
  let lexbuf = Lexing.from_string input in
  let ast = Parser.start Lexer.token lexbuf in
  let typecheck = Typechecker.typechecker ast (ref None) in
  if typecheck = Typechecker.NoneType then failwith "Failed typechecker pass. Expression type: None";
  Ast.eval ast (ref None)

let test_eval_string input expected_output _ =
  let result = eval_string input in
  assert_equal expected_output result ~printer:Ast.string_of_eval_result

let test_expected_failure input _ =
  try
    let _ = eval_string input in
    assert_failure "Expected type mismatch failure but evaluation succeeded."
  with
  | Failure _ ->
    ()
  | _ -> assert_failure "Expected type mismatch failure"


let suite =
  "integration_tests">:::
  [
    "test true evaluation">::test_eval_string "true\n" (Ast.Bool true);
    "test false evaluation">::test_eval_string "false\n" (Ast.Bool false);
    "test integer evaluation">::test_eval_string "10000\n" (Ast.Int 10000);
    "test addition">::test_eval_string "2+3\n" (Ast.Int 5);
    "test subtraction">::test_eval_string "2-3\n" (Ast.Int (-1));
    "test negation">::test_eval_string "-3\n" (Ast.Int (-3));
    "test sequence">::test_eval_string "1;2\n" (Ast.Int 2);
    "test multiplication">::test_eval_string "45*9\n" (Ast.Int 405);
    "test division">::test_eval_string "49/7\n" (Ast.Int 7);
    "test equality true">::test_eval_string "20 = 20\n" (Ast.Bool true);
    "test equality false">::test_eval_string "20 = 1\n" (Ast.Bool false);
    "test inequality true">::test_eval_string "40 != 20\n" (Ast.Bool true);
    "test inequality false">::test_eval_string "40 != 40\n" (Ast.Bool false);
    "test less than">::test_eval_string "98 < 6789\n" (Ast.Bool true);
    "test greater than">::test_eval_string "98 > 6789\n" (Ast.Bool false);
    "test less than or equal">::test_eval_string "19 <= 19\n" (Ast.Bool true);
    "test greater than or equal">::test_eval_string "19 >= 19\n" (Ast.Bool true);
    "test parentheses">::test_eval_string "(3)\n" (Ast.Int 3);
    "test boolean true">::test_eval_string "(true)\n" (Ast.Bool true);
    "test unit">::test_eval_string "(())\n" Ast.Unit;
    "test logical and true">::test_eval_string "true && true\n" (Ast.Bool true);
    "test logical and false">::test_eval_string "true && false\n" (Ast.Bool false);
    "test logical or true">::test_eval_string "true || true\n" (Ast.Bool true);
    "test logical or false">::test_eval_string "true || false\n" (Ast.Bool true);
    "test logical not true">::test_eval_string "~true\n" (Ast.Bool false);
    "test logical not false">::test_eval_string "~false\n" (Ast.Bool true);
    "test let integer">::test_eval_string "let x = 1 in x\n" (Ast.Int 1);
    "test let boolean true">::test_eval_string "let x = true in x\n" (Ast.Bool true);
    "test let boolean false">::test_eval_string "let x = false in x\n" (Ast.Bool false);
    "test let unit">::test_eval_string "let x = () in x\n" Ast.Unit;
    "test let new integer">::test_eval_string "let x = new 1 in x\n" (Ast.Ref (ref (Ast.Int 1)));
    "test let new boolean true">::test_eval_string "let x = new true in x\n" (Ast.Ref (ref (Ast.Bool true)));
    "test let new boolean false">::test_eval_string "let x = new false in x\n" (Ast.Ref (ref (Ast.Bool false)));
    "test let new unit">::test_eval_string "let x = new () in x\n" (Ast.Ref (ref Ast.Unit));
    "test type annotation integer">::test_eval_string "let x : int = 1 in x\n" (Ast.Int 1);
    "test type annotation boolean true">::test_eval_string "let x : bool = true in x\n" (Ast.Bool true);
    "test type annotation boolean false">::test_eval_string "let x : bool = false in x\n" (Ast.Bool false);
    "test type annotation unit">::test_eval_string "let x : unit = () in x\n" Ast.Unit;
    "test type annotation ref integer">::test_eval_string "let x : int ref = new 1 in x\n" (Ast.Ref (ref (Ast.Int 1)));
    "test type annotation ref boolean true">::test_eval_string "let x : bool ref = new true in x\n" (Ast.Ref (ref (Ast.Bool true)));
    "test type annotation ref boolean false">::test_eval_string "let x : bool ref = new false in x\n" (Ast.Ref (ref (Ast.Bool false)));
    (*"test type annotation ref unit">::test_eval_string "let x : unit ref = () in x\n" (Ast.Ref (ref Ast.Unit));*)
    "test type mismatch bool to int">::test_expected_failure "let x : bool = 1 in x\n";
    "test type mismatch int to bool">::test_expected_failure "let x : int = true in x\n";
    "test type mismatch int to bool">::test_expected_failure "let x : int = false in x\n";
    "test type mismatch unit to ref">::test_expected_failure "let x : int ref = () in x\n";
    "test new integer">::test_eval_string "let x = new 2 in !x\n" (Ast.Int 2);
    "test new integer assignment">::test_eval_string "let x = new 3 in x := 5\n" Ast.Unit;
    "test if-then true">::test_expected_failure "if true then 1 end\n";
    "test if-then-else true">::test_eval_string "if true then true else false end\n" (Ast.Bool true);
    "test if-then-else false">::test_eval_string "if true then false else true end\n" (Ast.Bool false);
    "test if-then-else unit">::test_eval_string "if true then () else () end\n" Ast.Unit;
    "test if-then-else new integer">::test_eval_string "if true then new 2 else new 5 end\n" (Ast.Ref (ref (Ast.Int 2)));
    "test if-then-else new nested">::test_eval_string "if true then 1 else 2 end\n" (Ast.Int 1);
    "test if-then-else new nested">::test_eval_string "if true then true else false end\n" (Ast.Bool true);
    "test if-then-else new nested">::test_eval_string "if true then false else true end\n" (Ast.Bool false);
  ]

let () =
  run_test_tt_main suite
