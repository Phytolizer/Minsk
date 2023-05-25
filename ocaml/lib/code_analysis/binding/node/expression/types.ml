open Runtime

type t =
  | Assignment of assignment
  | Binary of binary
  | Literal of literal
  | Unary of unary
  | Variable of variable

and assignment = {
  assignment_variable : Variable_symbol.t;
  assignment_expression : t;
}

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
  bop_syntax_kind : Token.kind;
  bop_kind : binary_operator_kind;
  bop_left_ty : Value.ty;
  bop_right_ty : Value.ty;
  bop_ty : Value.ty;
}

and literal = { literal_value : Value.t }
and unary = { unary_op : unary_op; unary_operand : t }
and unary_operator_kind = Identity | Negation | LogicalNegation

and unary_op = {
  uop_syntax_kind : Token.kind;
  uop_kind : unary_operator_kind;
  uop_operand_ty : Value.ty;
  uop_ty : Value.ty;
}

and variable = { variable_variable : Variable_symbol.t }

type kind =
  | KindAssignment
  | KindBinary
  | KindLiteral
  | KindUnary
  | KindVariable

let kind x =
  match x with
  | Assignment _ -> KindAssignment
  | Binary _ -> KindBinary
  | Literal _ -> KindLiteral
  | Unary _ -> KindUnary
  | Variable _ -> KindVariable

type expr = t
