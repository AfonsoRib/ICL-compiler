open Ast
(* open Frame *)
open Env

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
  | Getfield of string * string
  | Checkcast of string

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
          | Putfield (loc,t) -> "putfield " ^ loc ^ " " ^ t
          | Getfield (loc,t) -> "getfield " ^ loc ^ " " ^ t
          | Checkcast s -> "checkcast " ^ s
         )

let rec comp (expression : exp) (env : int environment option ref) : jvm list =
  match expression with
  | Fact (n, _) -> [Sipush n]
  | Statement (b, _) -> if b then [Sipush 1] else [Sipush 0]
  | Id (id, t) ->
     (* print_endline id; *)
     (* print_endline (Frame.type_to_string t); *)
     let findRes = Env.compFind !env id (-1) in
     let jumps = fst findRes and
         loc = (snd findRes)
     in
     let rec aux jmps =
       match jmps with
       | jmps when jmps < 0 -> failwith "invalid number of jumps"
       | jmps when jmps = 0 -> []
       | jmps when jmps > 0 ->
          let curFrame = !(Frame.counter_frames) - jmps in
          [Getfield ("frame_" ^ string_of_int curFrame ^ "/sl", "Lframe_" ^ string_of_int (curFrame-1) ^ ";")] @ aux (jmps-1)
       | _ -> failwith "unexpected value for jmps"

     in
     Aload 0 :: aux jumps @
       [Getfield ("frame_" ^ string_of_int (!(Frame.counter_frames) -1) ^ "/loc_"^ string_of_int loc, Frame.type_to_string t)]
  | Add (e1, e2, _) -> comp e1 env @ comp e2 env @ [Iadd]
  | Mult (e1, e2, _) -> comp e1 env @ comp e2 env @ [Imul]
  | Sub (e1, e2, _) -> comp e1 env @ comp e2 env @ [Isub]
  | Div (e1, e2, _) -> comp e1 env @ comp e2 env @ [Idiv]
  | Eq (e1, e2, _) -> let l1 = gen_label () in
                      let l2 = gen_label () in
                      comp e1 env @ comp e2 env @ [If_icmpne l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Ne (e1, e2, _) -> let l1 = gen_label () in
                      let l2 = gen_label () in
                      comp e1 env @ comp e2 env @ [If_icmpeq l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Le (e1, e2, _) -> let l1 = gen_label () in
                      let l2 = gen_label () in
                      comp e1 env @ comp e2 env @ [If_icmpgt l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Ge (e1, e2, _) -> let l1 = gen_label () in
                      let l2 = gen_label () in
                      comp e1 env @ comp e2 env @ [If_icmplt l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Lt (e1, e2, _) -> let l1 = gen_label () in
                      let l2 = gen_label () in
                      comp e1 env @ comp e2 env @ [If_icmpge l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | Gt (e1, e2, _) -> let l1 = gen_label () in
                      let l2 = gen_label () in
                      comp e1 env @ comp e2 env @ [If_icmple l1 ; Sipush 1; Goto l2; Label l1; Sipush 0; Label l2; Nop]
  | And(e1, e2, _) -> comp e1 env @ comp e2 env @ [Iand]
  | Or(e1, e2, _) -> comp e1 env @ comp e2 env @ [Ior]
  | Not(e, _) -> comp e env @ [Sipush 1; Ixor]
  | IfThenElse(e1,e2,e3,_) -> let c1 = comp e1 env and
                                  c2 = comp e2 env and
                                  c3 = comp e3 env and
                                  l1 = gen_label () and
                                  l2 = gen_label ()
                              in
                              c1 @ [Sipush 1; If_icmpne l1] @ c2 @ [Goto l2; Label l1; Nop] @ c3 @ [Label l2; Nop]
  | IfThen(e1,e2,_) -> let c1 = comp e1 env and
                           c2 = comp e2 env and
                           l1 = gen_label () and
                           l2 = gen_label ()
                       in
                       c1 @ [Sipush 1; If_icmpne l1] @ c2 @ [Goto l2; Label l1; Nop; Nop; Label l2; Nop]
  | Let(binds, expr, _) ->
     let n = Frame.gen_frame binds !env in
     let frame_number = "frame_" ^ n in
     let loc_id = ref 0 in
     let rec aux bindings n_env =
       match bindings with
       | [] -> []
       | (id, e1, t)::rest ->
          let c1 = comp e1 env in
          let ret = c1 @ [Putfield ("frame_" ^ string_of_int (!(Frame.counter_frames)-1) ^"/loc_" ^ string_of_int !loc_id, Frame.type_to_string t);] in
          Env.bind n_env id !loc_id;
          loc_id := !loc_id +1;
          ret @ (aux rest n_env)
     in
     env := Env.begin_scope !env;
     let vars = aux binds (!env) and
         res = comp expr env in
     env := Env.end_scope !env;
     
     [New frame_number;
      Dup;
      Invokespecial (frame_number ^ "/<init>()V");
      Dup;
      Aload 0;
      if !env = None then
        Putfield (frame_number^"/SL", "Ljava/lang/Object;")
      else
        Putfield (frame_number^"/SL", "Lframe_" ^ string_of_int((!Frame.counter_frames) - 1) ^ ";")
      ;
      Astore 0;
      Aload 0] @ vars @ res @ [Aload 0; Getfield (frame_number^"/SL", "Ljava/lang/Object;"); Astore 0]
  | String(s, _) -> [Ldc s]
  | UnitExp _ -> [Nop]
  | _ -> [Nop]
