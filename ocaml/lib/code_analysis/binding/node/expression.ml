open Runtime

type t = Binary of binary | Literal of literal | Unary of unary

and binary = {
  binary_left : t;
  binary_operator_kind : binary_operator_kind;
  binary_right : t;
}

and binary_operator_kind =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | LogicalAnd
  | LogicalOr

and literal = { literal_value : Value.t }
and unary = { unary_operator_kind : unary_operator_kind; unary_operand : t }
and unary_operator_kind = Identity | Negation | LogicalNegation

type kind = KindBinary | KindLiteral | KindUnary

let kind x =
  match x with
  | Binary _ -> KindBinary
  | Literal _ -> KindLiteral
  | Unary _ -> KindUnary

module Binary = struct
  type t = binary

  let make binary_left binary_operator_kind binary_right =
    { binary_left; binary_operator_kind; binary_right }

  let left b = b.binary_left
  let operator_kind b = b.binary_operator_kind
  let right b = b.binary_right
end

module Literal = struct
  type t = literal

  let make literal_value = { literal_value }
  let value v = v.literal_value
end

module Unary = struct
  type t = unary

  let make unary_operator_kind unary_operand =
    { unary_operator_kind; unary_operand }

  let operator_kind u = u.unary_operator_kind
  let operand u = u.unary_operand
end

let rec ty x =
  match x with
  | Binary b -> Binary.left b |> ty
  | Literal l -> Literal.value l |> Value.tyof
  | Unary u -> Unary.operand u |> ty
