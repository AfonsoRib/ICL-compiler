type typ=
  | IntType
  | FloatType
  | BoolType
  | UnitType
  | RefType of typ
  | NoneType
  | StringType
  | FunType of typ list * typ
