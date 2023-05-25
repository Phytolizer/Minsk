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

module Binary : sig
  type t = binary
  type op = binary_op

  val make : expr -> op -> expr -> t
  val bind_op : Syntax.Token.kind -> Value.ty -> Value.ty -> op option
  val left : t -> expr
  val op : t -> op
  val right : t -> expr

  module Op : sig
    val kind : op -> binary_operator_kind
    val result_ty : op -> Value.ty
  end
end = struct
  type t = binary
  type op = binary_op

  let make_same_op bop_syntax_kind bop_kind ty =
    {
      bop_syntax_kind;
      bop_kind;
      bop_left_ty = ty;
      bop_right_ty = ty;
      bop_ty = ty;
    }

  let make_matching_op bop_syntax_kind bop_kind operand_ty bop_ty =
    {
      bop_syntax_kind;
      bop_kind;
      bop_left_ty = operand_ty;
      bop_right_ty = operand_ty;
      bop_ty;
    }

  let bind_op syntax_kind left_ty right_ty =
    Array.find_opt
      (fun op ->
        op.bop_syntax_kind = syntax_kind
        && op.bop_left_ty = left_ty && op.bop_right_ty = right_ty)
      [|
        make_same_op Plus Addition TyInt;
        make_same_op Minus Subtraction TyInt;
        make_same_op Star Multiplication TyInt;
        make_same_op Slash Division TyInt;
        make_matching_op EqualsEquals Equality TyInt TyBool;
        make_matching_op BangEquals Inequality TyInt TyBool;
        (* boolean operators *)
        make_same_op AmpersandAmpersand LogicalAnd TyBool;
        make_same_op PipePipe LogicalOr TyBool;
        make_same_op EqualsEquals Equality TyBool;
        make_same_op BangEquals Inequality TyBool;
      |]

  let make binary_left binary_op binary_right =
    { binary_left; binary_op; binary_right }

  let left b = b.binary_left
  let op b = b.binary_op
  let right b = b.binary_right

  module Op = struct
    let kind op = op.bop_kind
    let result_ty op = op.bop_ty
  end
end

module Literal = struct
  type t = literal

  let make literal_value = { literal_value }
  let value v = v.literal_value
end

module Unary : sig
  type t = unary
  type op = unary_op

  val make : op -> expr -> t
  val bind_op : Syntax.Token.kind -> Value.ty -> op option
  val op : t -> op
  val operand : t -> expr

  module Op : sig
    val kind : op -> unary_operator_kind
    val result_ty : op -> Value.ty
  end
end = struct
  type t = unary
  type op = unary_op

  let make_same_op uop_syntax_kind uop_kind ty =
    { uop_syntax_kind; uop_kind; uop_operand_ty = ty; uop_ty = ty }

  let bind_op syntax_kind operand_ty =
    Array.find_opt
      (fun op ->
        op.uop_syntax_kind = syntax_kind && op.uop_operand_ty = operand_ty)
      [|
        make_same_op Plus Identity TyInt;
        make_same_op Minus Negation TyInt;
        make_same_op Bang LogicalNegation TyBool;
      |]

  let make unary_op unary_operand = { unary_op; unary_operand }
  let op u = u.unary_op
  let operand u = u.unary_operand

  module Op = struct
    let kind op = op.uop_kind
    let result_ty op = op.uop_ty
  end
end

let ty x =
  match x with
  | Binary b -> Binary.op b |> Binary.Op.result_ty
  | Literal l -> Literal.value l |> Value.tyof
  | Unary u -> Unary.op u |> Unary.Op.result_ty
