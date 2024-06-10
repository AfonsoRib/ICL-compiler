%token<string> NUM
%token<string> STRING
%token PLUS
%token MINUS
%token MULT
%token DIV
%token FPLUS
%token FMINUS
%token FMULT
%token FDIV
%token LPAR
%token RPAR
%token EOL
%token<string> ID
%token EQ
%token NE
%token LE
%token GE
%token LT
%token GT
%token TRUE
%token FALSE
%token LET
%token IN
%token AND
%token OR
%token NOT
%token NEW
%token DEREF
%token ASSIGN
%token WHILE
%token DO
%token END
%token IF
%token THEN
%token ELSE
%token PRINTLN
%token PRINT
%token SEMICOLON
%token UNIT
%token COLON
%token FUN
%token ARROW
%token<string> TYPE
%token COMMA
// %type <string list> type_refs_list


/* %left ARROW */
/* %left FUN */
/* %left ID */
/* %left COMMA */
%left SEMICOLON
%left PLUS
%left MINUS
%left MULT
%left DIV
%left FPLUS
%left FMINUS
%left FMULT
%left FDIV
%left EQ
%left NE
%left LE
%left GE
%left LT
%left GT
%left AND
%left OR
%left NOT
%left ASSIGN
%left DEREF
%left NEW
%left IN
%left PRINT
%left PRINTLN
%left LPAR
%start <Ast.exp> start
%%

start:
  | e=exp EOL
    { e }

exp:
  | TRUE
    { Statement(true, Types.NoneType) }
  | FALSE
    { Statement(false, Types.NoneType) }
  | n = NUM
    { if String.contains n '.'
      then FloatFact(float_of_string n, Types.NoneType)
      else Fact(int_of_string n, Types.NoneType)
    }
  | UNIT
    { UnitExp(Types.NoneType) }
  | s=STRING
    { String(s, Types.NoneType) }
  | id=ID
    { Id(id, Types.NoneType) }
  | e1=exp SEMICOLON e2=exp
    { Seq(e1,e2,Types.NoneType) }
  | e1=exp PLUS e2=exp
    { Add(e1, e2, Types.NoneType) }
  | e1=exp MINUS e2=exp
    { Sub(e1, e2, Types.NoneType) }
  | e1=exp MULT e2=exp
    { Mult(e1, e2, Types.NoneType) }
  | e1=exp DIV e2=exp
    { Div(e1, e2, Types.NoneType) }
  | e1=exp FPLUS e2=exp
    { Addf(e1, e2, Types.NoneType) }

  | e1=exp FMINUS e2=exp
    { Subf(e1, e2, Types.NoneType) }
  | e1=exp FMULT e2=exp
    { Multf(e1, e2, Types.NoneType) }
  | e1=exp FDIV e2=exp
    { Divf(e1, e2, Types.NoneType) }
  | e1=exp EQ e2=exp
    { Eq(e1, e2, Types.NoneType) }
  | e1=exp NE e2=exp
    { Ne(e1, e2, Types.NoneType) }
  | e1=exp GE e2=exp
    { Ge(e1, e2, Types.NoneType) }
  | e1=exp LE e2=exp
    { Le(e1, e2, Types.NoneType) }
  | e1=exp GT e2=exp
    { Gt(e1, e2, Types.NoneType) }
  | e1=exp LT e2=exp
    { Lt(e1, e2, Types.NoneType) }
  | MINUS n=NUM
    { Fact( - (int_of_string n), Types.NoneType) }
  | LPAR e=exp RPAR
    { e }
  | e1=exp AND e2=exp
    { And(e1, e2, Types.NoneType) }
  | e1=exp OR e2=exp
    { Or(e1, e2, Types.NoneType) }
  | NOT e1=exp
    { Not(e1, Types.NoneType) }
  | NEW e=exp
    { New(e, Types.NoneType) }
  | DEREF e=exp
    { Deref(e, Types.NoneType) }
  | e1=exp ASSIGN e2=exp
    { Assign(e1, e2, Types.NoneType) } 
  | PRINTLN e=exp
    { PrintLn(e, Types.NoneType) }
  | PRINT e=exp
    { Print(e, Types.NoneType) }
  | IF e1=exp THEN e2=exp ELSE e3=exp END
    { IfThenElse(e1,e2,e3, Types.NoneType) }
  | IF e1=exp THEN e2=exp END
    { IfThen(e1,e2, Types.NoneType) }
  | WHILE e1=exp DO e2=exp END
    { While(e1,e2, Types.NoneType)}
  | FUN a=arg_list ARROW e=exp END
    { Fun(a, e, Types.NoneType) }
  | LET bindings=let_bindings IN e=exp
    { Let(bindings, e, Types.NoneType) }
  | e1=exp LPAR args=app_arguments_list RPAR
    { App(e1,args,Types.NoneType)}
    



type_string:
  | TYPE
    {[$1]}
  | type_string TYPE
    { $1 @ [$2]}

t:
  | type_string
    { Typechecker.typ_str ($1) }
  | func
    { $1 }

t_star:
  | t
    { [$1] }
  | t_star COMMA t
    { $1 @ [$3] }

func:
  | LPAR t_star ARROW t RPAR
    { Types.FunType($2, $4) }

binding:
  | id=ID EQ e=exp
    { (id, e, Types.NoneType) }
  | id=ID COLON t EQ e=exp
    { (id, e, $3) }

let_bindings:
  | let_bindings binding
    { $1 @ [$2] }
  | binding
    { [$1] }

arg:
  | id=ID COLON t
    { (id, $3) }

arg_list:
  | arg_list arg
    { $1 @ [$2] }
  | arg
    { [$1] }

app_list:
  | exp
    { [$1] }
  | app_list COMMA exp
    { $1 @ [$3] }

app_arguments_list:
  | app_list
    { $1 }
  | { [] }


/* (fun x : int -> x end)() não funciona porque reconhece o segundo como unit mas (fun x : int -> x end)( ) já reconhece porque tem um espaço no centro*/
// app_arguments_list:
//   | { [] }
//   | app_list {$1}

// app_list:
//   | exp {[$1]}
//   | app_list COMMA exp {$1 @ [$3]}

// arg:
//   | id=ID COLON t=type_refs_list_list
//     { (id, t) }
//   // | id=ID COLON t=type_refs_list ARROW ret=type_refs_list

// arg_list:
//   | {[]}
//   | arg_list arg
//     {$1 @ [$2]}

// type_refs_list:
//     | TYPE
//         { [$1] }    
//     | type_refs_list TYPE
//         { $1 @ [$2] }

// type_args:
//   | type_args ARROW type_refs_list
//     { [Types.FunType(
//         $1
//       , Typechecker.typ_str (List.rev $3))] }
//   | type_refs_list
//     { [(Typechecker.typ_str (List.rev $1))] }
//   | type_args COMMA type_refs_list
//     { $1 @ [(Typechecker.typ_str (List.rev $3))] }


// type_refs_list_list:   
//     | type_refs_list
//         { (Typechecker.typ_str (List.rev $1)) }
//     | type_args ARROW type_refs_list
//         { Types.FunType($1,Typechecker.typ_str (List.rev $3))}
//      | LPAR type_refs_list_list RPAR
//         { $2 }

// let_bindings:
//   | binding
//     { [$1] }
//   | let_bindings binding
//     { $1 @ [$2] }

// binding:
//   | id=ID EQ e=exp
//     { (id, e, Types.NoneType) }
//   | id=ID COLON type_refs_list_list EQ  e=exp
//     { (id, e, $3) }
  // | id=ID COLON ts=type_refs_list_list ARROW ret=type_refs_list EQ e=exp
  //   { (id, e, FunType(ts, Typechecker.typ_str (List.rev ret))) }
    
    

// binding_type:
//   | id=ID COLON type_refs_list EQ e=exp
//     { (id, e, Typechecker.typ_str (List.rev $3)) }
  