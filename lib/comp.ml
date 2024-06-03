open Ast


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
  | Fadd
  | Fsub
  | Fmul
  | Fdiv
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
  | NewJvm of string
  | Dup
  | Invokespecial of string
  | Invokestatic of string
  | Invokevirtual of string
  | Aload of int
  | Astore of int
  | Putfield of string * string
  | Getfield of string * string
  | Checkcast of string
  | Pop
  | Getstatic of string * string
  | Invokeinterface of string * int


let getSubExprType e =
  match e with
  |Add (_,_,t) -> t
  |Sub (_,_,t) -> t
  |Mult (_,_,t) -> t
  |Div (_,_,t) -> t
  |Addf (_,_,t) -> t
  |Subf (_,_,t) -> t
  |Multf (_,_,t) -> t
  |Divf (_,_,t) -> t
  |Fact (_,t) -> t
  |FloatFact (_,t) -> t
  |Eq (_,_,t) -> t
  |Ne (_,_,t) -> t
  |Le (_,_,t) -> t
  |Ge (_,_,t) -> t
  |Lt (_,_,t) -> t
  |Gt (_,_,t) -> t
  |And (_,_,t) -> t
  |Or (_,_,t) -> t
  |Not (_,t) -> t
  |Statement (_,t) -> t
  |Let (_ ,_,t) -> t
  |Id (_,t) -> t
  |New (_,t) -> t
  |Deref (_,t) -> t
  |Assign (_,_,t) -> t
  |While (_,_,t) -> t
  |IfThenElse (_,_,_,t) -> t
  |IfThen (_,_,t) -> t
  |PrintLn (_,t) -> t
  |Print (_,t) -> t
  |Seq (_,_,t) -> t
  |UnitExp (t) -> t
  |String (_ ,t) -> t
  |Fun(_,_,t) -> t
  |App(_,_,t) -> t

let jvmString i =
  "\t" ^ (match i with
      | Iadd -> "iadd"
      | Isub -> "isub"
      | Imul -> "imul"
      | Idiv -> "idiv"
      | Fadd -> "fadd"
      | Fsub -> "fsub"
      | Fmul -> "fmul"
      | Fdiv -> "fdiv"
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
      | NewJvm s -> "new " ^ s
      | Dup -> "dup"
      | Invokespecial f -> "invokespecial " ^ f
      | Invokestatic f -> "invokestatic " ^ f
      | Invokevirtual f -> "invokevirtual " ^ f
      | Aload i -> "aload " ^ string_of_int i
      | Astore i -> "astore "  ^ string_of_int i
      | Putfield (loc,t) -> "putfield " ^ loc ^ " " ^ t
      | Getfield (loc,t) -> "getfield " ^ loc ^ " " ^ t
      | Checkcast s -> "checkcast " ^ s
      | Pop -> "pop"
      | Getstatic(s1,s2) -> "getstatic " ^ s1 ^ " " ^ s2
      | Invokeinterface (f,n_args) -> "invokeinterface " ^ f ^ " " ^ (string_of_int n_args)
    )

let rec comp (expression : exp) (env : int Frame.frame_env option ref) : jvm list =
  match expression with
  | Fact (n, _) -> [Sipush n]
  | FloatFact (f, _) -> [Ldc (string_of_float f)]
  | Statement (b, _) -> if b then [Sipush 1] else [Sipush 0]
  | Id (id, t) ->
    let temp = Frame.findJumpLocation !env id 0 in
    let jmps = fst temp in
    let loc_i = snd temp in
    let frame_i = Frame.findFrame !env id in (*esta funções pode ser posta dentro de outra. é só overhead*)
    let rec aux jmps (n_env : int Frame.frame_env) =
      (* TODO Option.get tem pode dar problemas*)
      if jmps = 0 then
        []
      else
        let prev = Option.get (n_env.prev) in
        Getfield("frame_" ^ string_of_int (n_env.id) ^ "/SL", "Lframe_" ^ string_of_int (prev.id) ^ ";" ) :: aux (jmps-1) prev
    in 
    let intermidiate = aux jmps (Option.get (!env)) in
    Aload 0
    ::  intermidiate @
    [Getfield ("frame_" ^ string_of_int (frame_i) ^ "/loc_" ^ string_of_int loc_i, Frame.type_to_string t);]

  | Add (e1, e2, _) -> comp e1 env @ comp e2 env @ [Iadd]
  | Mult (e1, e2, _) -> comp e1 env @ comp e2 env @ [Imul]
  | Sub (e1, e2, _) -> comp e1 env @ comp e2 env @ [Isub]
  | Div (e1, e2, _) -> comp e1 env @ comp e2 env @ [Idiv]
  | Addf (e1, e2, _) -> comp e1 env @ comp e2 env @ [Fadd] 
  | Multf (e1, e2, _) -> comp e1 env @ comp e2 env @ [Fmul]
  | Subf (e1, e2, _) -> comp e1 env @ comp e2 env @ [Fsub]
  | Divf (e1, e2, _) -> comp e1 env @ comp e2 env @ [Fdiv]
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
  | Let (binds, body, _ ) ->
    env := Frame.begin_scope !env;
    let var_counter = ref 0 in
    let vars = List.map (fun (id, e1, t) ->
        let frame = Option.get !env in
        let c1 = comp e1 env in
        let loc = !var_counter in
        var_counter := !var_counter + 1;
        Frame.bind !env id loc t;
        Aload 0 :: c1 @ [Putfield ("frame_" ^ string_of_int frame.id ^ "/loc_" ^ string_of_int loc, Frame.type_to_string t)]
      ) binds in
    let res = comp body env in
    let frame_id = (Option.get !env).id in
    let frame_string = "frame_" ^ string_of_int frame_id in
    Frame.create_frame_file (Option.get (!env));
    env := Frame.end_scope !env;
    let frameType =
      if !env = None then
        "Ljava/lang/Object;"
      else
        "Lframe_" ^ string_of_int (Option.get (!env)).id ^ ";" in
    [NewJvm frame_string;
     Dup;
     Invokespecial (frame_string ^ "/<init>()V");
     Dup;
     Aload 0;
     Putfield (frame_string^"/SL", frameType);
     Astore 0;
    ] @ List.flatten vars @ res @ [Aload 0; Getfield (frame_string^"/SL", frameType); Astore 0]  
  | Seq(e1,e2,_) -> let c1 = comp e1 env
    and c2 = comp e2 env in
    let t1 = getSubExprType e1 in
    let aux = if t1 = UnitType then [] else [Pop] in
    c1 @ aux @ c2
  | New(e1,t) ->
    let c1 = comp e1 env in
    let typeName = Ref.gen_ref t in
    [NewJvm typeName;
     Dup;
     Invokespecial (typeName ^ "/<init>()V");
     Dup;
    ] @ c1 @ [Putfield (typeName^"/value", Ref.string_of_ref_subtype t)]
  | Deref(e1,t) ->
    let c1 = comp e1 env in
    let loc = (Ref.string_of_type (RefType(t))) ^ "/value"
    in 
    c1 @ [Getfield (loc, Frame.type_to_string (t))]
  | Assign(e1,e2,_) ->
    let c1 = comp e1 env in
    let c2 = comp e2 env in
    let loc = Ref.string_of_type (getSubExprType e1) ^ "/value"
    and t1 = Ref.string_of_ref_subtype (getSubExprType e1) in
    c1 @ c2 @ [Putfield (loc, t1)]
  | While(e1,e2,_) ->
    let c1 = comp e1 env and
    c2 = comp e2 env and
    l1 = gen_label () and
    l2 = gen_label ()
    in
    [Label l1] @ c1 @ [Sipush 1; If_icmpne l2;] @ c2 @ [Goto l1; Label l2; Nop]
  | Print(e1,_) ->
    let printType t =
      (match t with
       | Types.IntType -> Invokestatic "java/lang/String/valueOf(I)Ljava/lang/String;"
       | Types.FloatType -> Invokestatic  "java/lang/String/valueOf(F)Ljava/lang/String;"
       | Types.UnitType | Types.StringType -> Invokestatic "java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;"
       | Types.BoolType -> Invokestatic "java/lang/String/valueOf(Z)Ljava/lang/String;"
       | _ -> Nop)
      :: [Invokevirtual "java/io/PrintStream/print(Ljava/lang/String;)V"]
    in
    let c1 = comp e1 env in
    let t1 = getSubExprType e1 in
    Getstatic ("java/lang/System/out", "Ljava/io/PrintStream;") :: c1 @ printType t1
  | PrintLn(e1,_) ->
    let printType t =
      (match t with
       | Types.IntType -> Invokestatic "java/lang/String/valueOf(I)Ljava/lang/String;"
       | Types.FloatType -> Invokestatic  "java/lang/String/valueOf(F)Ljava/lang/String;"
       | Types.UnitType | Types.StringType -> Invokestatic "java/lang/String/valueOf(Ljava/lang/Object;)Ljava/lang/String;"
       | Types.BoolType -> Invokestatic "java/lang/String/valueOf(Z)Ljava/lang/String;"
       | _ -> Nop)
      :: [Invokevirtual "java/io/PrintStream/println(Ljava/lang/String;)V"]
    in
    let c1 = comp e1 env in
    let t1 = getSubExprType e1 in
    Getstatic ("java/lang/System/out", "Ljava/io/PrintStream;") :: c1 @ printType t1
  | Fun(args, body, t) ->    
    let clsr_t = match t with
      | Types.FunType(_,t1) -> t1
      | _ -> failwith "Fun type error"
    in
    let args_t = List.map (fun (_,t) -> t ) args in
    env := Frame.begin_scope !env;
    (*TODO create frame file*)
    let counter = ref 0 in
    List.iter (fun (id, t) -> Frame.bind !env id !counter t; counter := !counter + 1) args;
    Frame.create_frame_file (Option.get !env);
    let compiled_body = comp body env in
    let compiled_body_str = List.map jvmString compiled_body in

    let closure_classname = Closure.create_closure_file args_t clsr_t !env compiled_body_str in
    env := Frame.end_scope !env;
    [NewJvm closure_classname;
     Dup;
     Invokespecial (closure_classname ^ "/<init>()V");
     Dup;
     Aload 0;
     Putfield (closure_classname^"/SL", 
               if !env = None then
                 "Ljava/lang/Object;"
               else
                 "Lframe_" ^ string_of_int (Option.get !env).id ^ ";");
    ]
  | App(e1, args, _) -> 
    let subExprType = (getSubExprType e1) in
    let args_t = match subExprType with
      | Types.FunType(args_t,_) -> args_t
      | _ -> failwith "Not Fun_type in App"
    in
    let ret_type = match subExprType with
      | Types.FunType(_,ret) -> ret
      | _ -> failwith "Not Fun_type in App ret"
    in 
    let interface_name = "closure_interface_" ^
                         String.concat "_" (List.map Ref.string_of_type  args_t)
                         ^ "_" ^ Ref.string_of_type ret_type in
    let c1 = comp e1 env in
    let c2 = List.flatten (List.map (fun x -> comp x env) args) in
    c1 @ c2 @ 
    [Invokeinterface (interface_name ^ "/apply(" ^ (String.concat "" (List.map Frame.type_to_string args_t)) ^ ")" ^ Frame.type_to_string ret_type,(List.length args) +1 )]
  | String(s, _) -> [Ldc s]
  | UnitExp _ -> [Nop]          (* criar uma classe para units *)

(* | _ -> [Nop] *)
