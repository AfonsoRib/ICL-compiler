open OUnit2
open Icl
open Ast

let empty_env = ref (Some (Env.create_environment None))

let test_addition _ =
  assert_equal
    ~printer:string_of_eval_result
    (Int 5)
    (eval (Add (Fact 2, Fact 3)) empty_env)

let test_subtraction _ =
  assert_equal
    ~printer:string_of_eval_result
    (Int 1)
    (eval (Sub (Fact 3, Fact 2)) empty_env)

let test_multiplication _ =
  assert_equal
    ~printer:string_of_eval_result
    (Int 6)
    (eval (Mult (Fact 2, Fact 3)) empty_env)

let test_division _ =
  assert_equal
    ~printer:string_of_eval_result
    (Int 2)
    (eval (Div (Fact 6, Fact 3)) empty_env)

let test_inequality _ =
  assert_equal
    ~printer:string_of_eval_result
    (Bool true)
    (eval (Ne (Fact 5, Fact 3)) empty_env)

let test_logical_and _ =
  assert_equal
    ~printer:string_of_eval_result
    (Bool true)
    (eval (And (Statement true, Statement true)) empty_env)

let test_logical_or _ =
  assert_equal
    ~printer:string_of_eval_result
    (Bool true)
    (eval (Or (Statement false, Statement true)) empty_env)

let test_logical_not _ =
  assert_equal
    ~printer:string_of_eval_result
    (Bool false)
    (eval (Not (Statement true)) empty_env)

let suite =
  "Test Eval">:::[
    "test addition">::test_addition;
    "test subtraction">::test_subtraction;
    "test multiplication">::test_multiplication;
    "test division">::test_division;
    "test inequality">::test_inequality;
    "test logical AND">::test_logical_and;
    "test logical OR">::test_logical_or;
    "test logical NOT">::test_logical_not;
  ]

let () =
  run_test_tt_main suite
