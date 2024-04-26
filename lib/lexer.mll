{
  open Parser
}

let skip = [' ' '\t' '\r']
let integer = ['0'-'9']+
let decimal = ['0'-'9']* '.' ['0'-'9']+
let num = integer | decimal
let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let strings = '\"' [^ '\"']* '\"'

rule token = parse
  |skip+ { token lexbuf }
  |num as n { NUM n }
  |'+' { PLUS }
  |'-' { MINUS }
  |'*' { MULT }
  |'/' { DIV }
  |'(' { LPAR }
  |')' { RPAR }
  |'=' { EQ }
  |"!=" { NE }
  |'<' { LT }
  |'>' { GT }
  |"<=" { LE }
  |">=" { GE }
  |"&&" { AND }
  |"||" { OR }
  |'~' { NOT }
  |'\n' { EOL }
  |"true"   { TRUE }
  |"false"  { FALSE }
  |"let" { LET }
  |"in" { IN }
  |"new" { NEW }
  |'!' { DEREF }
  |":=" { ASSIGN }
  |"while" { WHILE }
  |"do" { DO }
  |"end" { END }
  |"if" { IF }
  |"then" { THEN }
  |"else" { ELSE }
  |"end" { END }
  |"println" { PRINTLN }
  |"print" { PRINT }
  |';' { SEMICOLON }
  |':' { COLON }
  |"()"{ UNIT }
  |"bool" { TYPE "bool" }
  |"int" { TYPE "int" }
  |"float" { TYPE "float" }
  |"ref" { TYPE "ref" }
  |"unit" { TYPE "unit" }
  |"string" {TYPE "string"}
  |strings as s { STRING s }
  |identifier as id { ID id }
