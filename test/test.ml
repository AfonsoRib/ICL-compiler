open OUnit2
open Icl

let eval_string input =
  let lexbuf = Lexing.from_string input in
  let ast1 = Parser.start Lexer.token lexbuf in
  let typecheck = (Typechecker.typechecker ast1 (ref None)) in
  let typ = fst typecheck in
  let ast2 = snd typecheck in (*isto quebra os testes, don't know why*)
  if typ = Types.NoneType then failwith "Failed typechecker pass. Expression type: None"; (*TODO: mudar isto*)
  Ast.eval ast2 (ref None)

let test_eval_string input expected_output _ =
  let result = eval_string input in
  assert_equal expected_output result ~printer:Ast.string_of_eval_result

let test_expected_failure input _ =
  try
    let _ = eval_string input in
    assert_failure "Expected type mismatch failure but succeeded."
  with
    | Failure _ -> ()
    | _ -> assert_failure "Expected type mismatch failure"

let suite =
  "integration_tests">:::
  [
    "test true evaluation">::test_eval_string "true\n" (Ast.Bool true);
    "test false evaluation">::test_eval_string "false\n" (Ast.Bool false);
    "test integer evaluation">::test_eval_string "10000\n" (Ast.Int 10000);
    "test float evaluation">::test_eval_string "10000.5\n" (Ast.Float 10000.5);
    "test int addition">::test_eval_string "2+3\n" (Ast.Int 5);
    "test float addition">::test_eval_string"1.1+.8.6\n" (Ast.Float 9.7);
    "test int subtraction">::test_eval_string "2-3\n" (Ast.Int (-1));
    "test float subtraction">::test_eval_string "1.1-.8.6\n" (Ast.Float (-7.5));
    "test negation">::test_eval_string "-3\n" (Ast.Int (-3));
    "test sequence">::test_eval_string "1;2\n" (Ast.Int 2);
    "test multiplication">::test_eval_string "45*9\n" (Ast.Int 405);
    "test multiplication">::test_eval_string "45.0*.9.0\n" (Ast.Float 405.0);
    "test int division">::test_eval_string "49/7\n" (Ast.Int 7);
    "test float division">::test_eval_string "49.0/.7.0\n" (Ast.Float 7.0);
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
    "test let float">::test_eval_string "let x = 1.0 in x\n" (Ast.Float 1.0);
    "test let boolean true">::test_eval_string "let x = true in x\n" (Ast.Bool true);
    "test let boolean false">::test_eval_string "let x = false in x\n" (Ast.Bool false);
    "test let unit">::test_eval_string "let x = () in x\n" Ast.Unit;
    "test let new integer">::test_eval_string "let x = new 1 in x\n" (Ast.Ref (ref (Ast.Int 1)));
    "test let new float">::test_eval_string "let x = new 1.0 in x\n" (Ast.Ref (ref (Ast.Float 1.0)));
    "test let new boolean true">::test_eval_string "let x = new true in x\n" (Ast.Ref (ref (Ast.Bool true)));
    "test let new boolean false">::test_eval_string "let x = new false in x\n" (Ast.Ref (ref (Ast.Bool false)));
    "test let new unit">::test_eval_string "let x = new () in x\n" (Ast.Ref (ref Ast.Unit));
    "test type annotation integer">::test_eval_string "let x : int = 1 in x\n" (Ast.Int 1);
    "test type annotation float">::test_eval_string "let x : float = 1.0 in x\n" (Ast.Float 1.0);
    "test type annotation boolean true">::test_eval_string "let x : bool = true in x\n" (Ast.Bool true);
    "test type annotation boolean false">::test_eval_string "let x : bool = false in x\n" (Ast.Bool false);
    "test type annotation unit">::test_eval_string "let x : unit = () in x\n" Ast.Unit;
    "test type annotation ref integer">::test_eval_string "let x : ref int = new 1 in x\n" (Ast.Ref (ref (Ast.Int 1)));
    "test type annotation ref float">::test_eval_string "let x : ref float = new 1.0 in x\n" (Ast.Ref (ref (Ast.Float 1.0)));
    "test type annotation ref boolean true">::test_eval_string "let x : ref bool = new true in x\n" (Ast.Ref (ref (Ast.Bool true)));
    "test type annotation ref boolean false">::test_eval_string "let x : ref bool = new false in x\n" (Ast.Ref (ref (Ast.Bool false)));
    "test type missmatch ref unit">::test_expected_failure "let x : unit ref = () in x\n";
    "test type mismatch bool to int">::test_expected_failure "let x : bool = 1 in x\n";
    "test type mismatch int to bool">::test_expected_failure "let x : int = true in x\n";
    "test type mismatch int to bool">::test_expected_failure "let x : int = false in x\n";
    "test type mismatch unit to ref">::test_expected_failure "let x : int ref = () in x\n";
    "test type mismatch float to int">::test_expected_failure "1.1 < 1\n";
    "test type mismatch int to float">::test_expected_failure "2 = 5.4\n";
    "test new integer">::test_eval_string "let x = new 2 in !x\n" (Ast.Int 2);
    "test new float">::test_eval_string "let x = new 2.0 in !x\n" (Ast.Float 2.0);
    "test new integer assignment">::test_eval_string "let x = new 3 in x := 5\n" Ast.Unit;
    "test new float assignment">::test_eval_string "let x = new 3.0 in x := 5.0\n" Ast.Unit;
    "test if-then true">::test_expected_failure "if true then 1 end\n";
    "test if-then-else true">::test_eval_string "if true then true else false end\n" (Ast.Bool true);
    "test if-then-else false">::test_eval_string "if true then false else true end\n" (Ast.Bool false);
    "test if-then-else unit">::test_eval_string "if true then () else () end\n" Ast.Unit;
    "test if-then-else new integer">::test_eval_string "if true then new 2 else new 5 end\n" (Ast.Ref (ref (Ast.Int 2)));
    "test if-then-else new float">::test_eval_string "if true then new 2.0 else new 5.0 end\n" (Ast.Ref (ref (Ast.Float 2.0)));
    "test if-then-else new nested 1">::test_eval_string "if true then 1 else 2 end\n" (Ast.Int 1);
    "test if-then-else new nested 2">::test_eval_string "if true then 1.0 else 2.0 end\n" (Ast.Float 1.0);
    "test if-then-else new nested 3">::test_eval_string "if true then true else false end\n" (Ast.Bool true);
    "test if-then-else new nested 4">::test_eval_string "if true then false else true end\n" (Ast.Bool false);
    "test print">::test_eval_string "print 10\n" Ast.Unit;
    "test println">::test_eval_string "println 10\n" Ast.Unit;
    "test while loop with prints">::test_eval_string "let x = new 10 in (while !x > 0 do ((println !x) ; x := (!x - 1)) end)\n" Ast.Unit;
    "test sequence">::test_eval_string "let x = new 3 in ((x := (!x - 1)) ; !x)\n" (Ast.Int 2);
    "test recursive function">::test_eval_string"let f : (int, int -> int) = (fun x:int y:int -> (if (x>0) then (f(x-1,y)) else y end) end) in println (f(2,10))\n" Ast.Unit;
    "test passing functions as arguments">::test_eval_string"let f : ((int -> int) -> int)  = (fun x : (int->int) -> (x(2)) end) in 1\n" (Ast.Int 1);
    "test function to print unit">::test_eval_string"let x : (unit->unit) = (fun x:unit -> println(x) end) in x(())\n" Ast.Unit;
    "test passing two arguments">::test_eval_string"println(println((fun x: (int,int -> int  ) -> 2 end)((fun x:int y:int -> (x+y) end))))\n" Ast.Unit;
    "test variable shadowing 1">::test_eval_string "let x = 5 in let x = 10 in x\n" (Ast.Int 10);
    "test variable shadowing 2">::test_eval_string "let x = 5 in let x = x + 1 in x\n" (Ast.Int 6);
    "test function composition 1">::test_eval_string "let f = (fun x:int -> x + 1 end) in let g = (fun y:int -> y * 2 end) in g(f(3))\n" (Ast.Int 8);
    "test function composition 2">::test_eval_string "let f = (fun x:int -> x + 1 end) in let g = (fun y:int -> y * 2 end) in let h = (fun z:int -> z - 1 end) in h(g(f(3)))\n" (Ast.Int 7);
    "test higher-order function 1">::test_eval_string "let apply = (fun f:(int -> int) x:int -> f(x) end) in let increment = (fun y:int -> y + 1 end) in apply(increment, 5)\n" (Ast.Int 6);
    "test higher-order function 2">::test_eval_string "let applytwice : ((int -> int), int -> int) = (fun f:(int -> int) x:int -> f(f(x)) end) in  let double = (fun y:int -> y * 2 end) in applytwice(double, 3)\n" (Ast.Int 12);
    "test recursive function 1">::test_eval_string "let fact : (int -> int) = (fun n:int -> if (n = 0) then 1 else n * fact(n - 1) end end) in fact(5)\n" (Ast.Int 120);
    "test recursive function 2">::test_eval_string "let fib : (int -> int) = (fun n:int -> if n <= 1 then n else fib(n - 1) + fib(n - 2) end end) in fib(10)\n" (Ast.Int 55);
    "test complex conditional 1">::test_eval_string "if true then if false then 1 else 2 end else 3 end\n" (Ast.Int 2);
    "test complex conditional 2">::test_eval_string "if false then if true then 1 else 2 end else 3 end\n" (Ast.Int 3);
    "test nested scopes 1">::test_eval_string "let x = 1 in let y = 2 in let z = x + y in z\n" (Ast.Int 3);
    "test nested scopes 2">::test_eval_string "let x = 1 in let f = (fun y:int -> x + y end) in f(10)\n" (Ast.Int 11);
    "test type mismatch wrong assignment">::test_expected_failure "let x = new 1 in !x := \"wrong type\"\n";
    "test division by zero">::test_expected_failure "let x = 1 / 0 in x\n";                                           
    "test type mismatch function application">::test_expected_failure "let f = (fun x:int -> x + 1 end) in f(\"string\")\n";
    "test logical and/or 1">::test_eval_string "let x = true && (false || true) in x\n" (Ast.Bool true);
    "test logical and/or 2">::test_eval_string "let x = false || (true && false) in x\n" (Ast.Bool false);
    "test complex while loop">::test_eval_string "let x = new 0 in while ((!x) < 10) do (println(!x); x := (!x + 1)) end\n" Ast.Unit;
    "test complex expression 1">::test_eval_string "let x = 1 + 2 * 3 - 4 / 2 in x\n" (Ast.Int 5);
    "test complex expression 2">::test_eval_string "let x = 1.5 +. 2.5 *. 3.5 -. 4.5 /. 2.5 in x\n" (Ast.Float 8.45);
    "test nested references 1">::test_eval_string "let x = new (new 1) in !(x)\n" (Ast.Ref (ref (Ast.Int 1)));
    "test nested references 2">::test_eval_string "let x = new (new true) in !(!x)\n" (Ast.Bool true);
  ]

let () =
  run_test_tt_main suite
