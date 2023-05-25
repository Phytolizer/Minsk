open Runtime

type t = Binary of binary | Literal of literal | Unary of unary
and binary = { binary_left : t; binary_op : binary_op; binary_right : t }

and binary_operator_kind =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | LogicalAnd
  | LogicalOr
  | Equality
  | Inequality

and binary_op = {
  bop_syntax_kind : Syntax.Token.kind;
  bop_kind : binary_operator_kind;
  bop_left_ty : Value.ty;
  bop_right_ty : Value.ty;
  bop_ty : Value.ty;
}

and literal = { literal_value : Value.t }
and unary = { unary_op : unary_op; unary_operand : t }
and unary_operator_kind = Identity | Negation | LogicalNegation

and unary_op = {
  uop_syntax_kind : Syntax.Token.kind;
  uop_kind : unary_operator_kind;
  uop_operand_ty : Value.ty;
  uop_ty : Value.ty;
}

type kind = KindBinary | KindLiteral | KindUnary

let kind x =
  match x with
  | Binary _ -> KindBinary
  | Literal _ -> KindLiteral
  | Unary _ -> KindUnary

type expr = t
