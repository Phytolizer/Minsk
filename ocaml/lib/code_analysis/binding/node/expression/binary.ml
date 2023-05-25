open Types

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
