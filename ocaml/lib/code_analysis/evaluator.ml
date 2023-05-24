open Binding.Node.Expression

let rec evaluate_expression node =
  match node with
  | Literal l -> l.literal_value
  | Binary b -> (
      let left = evaluate_expression b.binary_left in
      let right = evaluate_expression b.binary_right in
      match b.binary_operator_kind with
      | BinaryAddition -> left + right
      | BinarySubtraction -> left - right
      | BinaryMultiplication -> left * right
      | BinaryDivision -> left / right)
  | Unary u -> (
      let operand = evaluate_expression u.unary_operand in
      match u.unary_operator_kind with
      | UnaryIdentity -> operand
      | UnaryNegation -> -operand)

let evaluate root = evaluate_expression root
