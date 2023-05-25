open Types

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
