open Ast

let counter = ref 0

let gen_number () =
  let fresh_number = !counter in
  counter := fresh_number + 1;
  fresh_number

type jvm =
  | Iadd 
  | Isub 
  | Imul 
  | Idiv
  | Iand
  | Ior
  | Ixor
  | If_icmpeq of string
  | If_icmpne of string
  | If_icmpge of string
  | If_icmpgt of string
  | If_icmple of string
  | If_icmplt of string
  | Sipush of int
  | Iconst_1
  | Iconst_0
  | Istore_0
  | Istore_1
  | Label of string * jvm
  | Nop
  | Goto of string
 

let rec jvmString = function
  | Iadd -> "iadd"
  | Isub -> "isub"
  | Imul -> "imul"
  | Idiv -> "idiv"
  | Iand -> "iand"
  | Ior -> "ior"
  | Ixor -> "ixor"
  | If_icmpeq label -> "if_icmpeq " ^ label
  | If_icmpne label -> "if_icmpne " ^ label
  | If_icmpge label -> "if_icmpge " ^ label
  | If_icmpgt label -> "if_icmpgt " ^ label
  | If_icmple label -> "if_icmple " ^ label
  | If_icmplt label -> "if_icmplt " ^ label
  | Sipush x -> "sipush " ^ string_of_int x
  | Iconst_1 -> "iconst_1"
  | Iconst_0 -> "iconst_0"
  | Istore_0 -> "istore_0"
  | Istore_1 -> "istore_1"
  | Label (label,inst) ->  label ^ ":\n" ^ (jvmString inst)
  | Goto label -> "goto " ^ label
  | Nop -> "nop"


let rec comp (expression : exp) (* (environment : env) *) : jvm list =
  match expression with
  | Fact n -> [Sipush n]
  | Statement b -> if b then [Sipush 1] else [Sipush 0]
  | Add (e1, e2) -> comp e1 @ comp e2 @ [Iadd]
  | Mult (e1, e2) -> comp e1 @ comp e2 @ [Imul]
  | Sub (e1, e2) -> comp e1 @ comp e2 @ [Isub]
  | Div (e1, e2) -> comp e1 @ comp e2 @ [Idiv]
  | Eq (e1, e2) -> let l1 = "L" ^ (string_of_int (gen_number ())) in
                   let l2 = "L" ^ (string_of_int (gen_number ())) in
                   comp e1 @ comp e2 @ [If_icmpne l1 ; Sipush 1; Goto l2; Label (l1,Sipush 0); Label (l2, Nop)]
  | Ne (e1, e2) -> let l1 = "L" ^ (string_of_int (gen_number ())) in
                   let l2 = "L" ^ (string_of_int (gen_number ())) in
                   comp e1 @ comp e2 @ [If_icmpeq l1 ; Sipush 1; Goto l2; Label (l1,Sipush 0); Label (l2, Nop)]
  | Le (e1, e2) -> let l1 = "L" ^ (string_of_int (gen_number ())) in
                   let l2 = "L" ^ (string_of_int (gen_number ())) in
                   comp e1 @ comp e2 @ [If_icmpgt l1 ; Sipush 1; Goto l2; Label (l1,Sipush 0); Label (l2, Nop)]
  | Ge (e1, e2) -> let l1 = "L" ^ (string_of_int (gen_number ())) in
                   let l2 = "L" ^ (string_of_int (gen_number ())) in
                   comp e1 @ comp e2 @ [If_icmplt l1 ; Sipush 1; Goto l2; Label (l1,Sipush 0); Label (l2, Nop)]
  | Lt (e1, e2) -> let l1 = "L" ^ (string_of_int (gen_number ())) in
                   let l2 = "L" ^ (string_of_int (gen_number ())) in
                   comp e1 @ comp e2 @ [If_icmpge l1 ; Sipush 1; Goto l2; Label (l1,Sipush 0); Label (l2, Nop)]
  | Gt (e1, e2) -> let l1 = "L" ^ (string_of_int (gen_number ())) in
                   let l2 = "L" ^ (string_of_int (gen_number ())) in
                   comp e1 @ comp e2 @ [If_icmple l1 ; Sipush 1; Goto l2; Label (l1,Sipush 0); Label (l2, Nop)]
  | And(e1, e2) -> comp e1 @ comp e2 @ [Iand]
  | Or(e1, e2) -> comp e1 @ comp e2 @ [Ior]
  | Not(e) -> comp e @ [Sipush 1; Ixor]
  | _ -> [Nop]
