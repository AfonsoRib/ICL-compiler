open Ast
(* open Frame *)

let counter_labels = ref 0

let gen_number_label () =
  let fresh_number = !counter_labels in
  counter_labels := fresh_number + 1;
  fresh_number

let gen_label () = "L" ^ (string_of_int (gen_number_label ()))

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
  | Label of string
  | Nop
  | Goto of string
  | Ldc of string
  | New of string
  | Dup
  | Invokespecial of string
  | Aload of int
  | Astore of int
  | Putfield of string * string
  (* | GetField of string * string *)

let jvmString i =
  "\t" ^ (match i with
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
  | Label label ->  label ^ ":\n"
  | Goto label -> "goto " ^ label
  | Nop -> "nop"
  | Ldc s -> "ldc " ^ s
  | New s -> "new " ^ s
  | Dup -> "dup"
  | Invokespecial f -> "invokespecial " ^ f
  | Aload i -> "aload " ^ string_of_int i
  | Astore i -> "astore "  ^ string_of_int i
  | Putfield(var,t) -> "putfield " ^ var ^ " " ^ t
         )

let rec comp (expression : exp) (* (environment : env) *) : jvm list =
  match expression with
  | Fact n -> [Sipush n]
  | Statement b -> if b then [Sipush 1] else [Sipush 0]
  | Id _ -> [Nop]
  | String s -> [Ldc s]
  | Add (e1, e2) -> comp e1 @ comp e2 @ [Iadd]
  | Mult (e1, e2) -> comp e1 @ comp e2 @ [Imul]
  | Sub (e1, e2) -> comp e1 @ comp e2 @ [Isub]
  | Div (e1, e2) -> comp e1 @ comp e2 @ [Idiv]
  | Eq (e1, e2) -> let l1 = gen_label () in
                   let l2 = gen_label () in
                   comp e1 @ comp e2 @ [If_icmpne l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Ne (e1, e2) -> let l1 = gen_label () in
                   let l2 = gen_label () in
                   comp e1 @ comp e2 @ [If_icmpeq l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Le (e1, e2) -> let l1 = gen_label () in
                   let l2 = gen_label () in
                   comp e1 @ comp e2 @ [If_icmpgt l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Ge (e1, e2) -> let l1 = gen_label () in
                   let l2 = gen_label () in
                   comp e1 @ comp e2 @ [If_icmplt l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Lt (e1, e2) -> let l1 = gen_label () in
                   let l2 = gen_label () in
                   comp e1 @ comp e2 @ [If_icmpge l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Gt (e1, e2) -> let l1 = gen_label () in
                   let l2 = gen_label () in
                   comp e1 @ comp e2 @ [If_icmple l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | And(e1, e2) -> comp e1 @ comp e2 @ [Iand]
  | Or(e1, e2) -> comp e1 @ comp e2 @ [Ior]
  | Not(e) -> comp e @ [Sipush 1; Ixor]
  | IfThenElse(e1,e2,e3) -> let c1 = comp e1 and
                                c2 = comp e2 and
                                c3 = comp e3 and
                                l1 = gen_label () and
                                l2 = gen_label ()
                            in
                            c1 @ [Sipush 1; If_icmpne l1] @ c2 @ [Goto l2; Label l1; Nop] @ c3 @ [Label l2; Nop]
  | IfThen(e1,e2) -> let c1 = comp e1 and
                         c2 = comp e2 and
                         l1 = gen_label () and
                         l2 = gen_label ()
                     in
                     c1 @ [Sipush 1; If_icmpne l1] @ c2 @ [Goto l2; Label l1; Nop; Nop; Label l2; Nop]
  (* |let(binds, e) -> *)
  (*     Frame.gen_class *)
  (*     (\*make frame file*\) *)

  | UnitExp -> [Nop]

  | _ -> [Nop]
