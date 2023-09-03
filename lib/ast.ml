type expr =
  | EVar of string
  | ELit of lit
  | ELambda of string * expr
  | EApp of expr * expr
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
[@@deriving show]

and lit =
  | LUnit
  | LInt of int
  | LBool of bool
[@@deriving show]
