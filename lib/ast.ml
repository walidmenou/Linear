type ident = string (* identifier of variables *)
type binary_op = Add | Sub | Mul | Div | Equal | LessThan | LessThanOrEq
type uop = Negate
type typ = Int | Bool | Unit | Comma | Arrow
type literal = Int of int | Bool of bool | Unit

type expr =
  | Val of ident
  | Lit of literal
  | BinOp of expr * binary_op * expr
  | Let of ident * expr * expr
  | If of expr * expr * expr
  | Fun of ident * expr
  | App of expr * expr
  | Rec of ident * expr
