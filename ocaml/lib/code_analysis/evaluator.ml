open Runtime.Value
open Binding.Node.Expression

let rec evaluate_expression node =
  match node with
  | Literal l -> l.literal_value
  | Binary b -> (
      let left = evaluate_expression b.binary_left in
      let right = evaluate_expression b.binary_right in
      match b.binary_operator_kind with
      | BinaryAddition -> Int (get_int left + get_int right)
      | BinarySubtraction -> Int (get_int left - get_int right)
      | BinaryMultiplication -> Int (get_int left * get_int right)
      | BinaryDivision -> Int (get_int left / get_int right))
  | Unary u -> (
      let operand = evaluate_expression u.unary_operand in
      match u.unary_operator_kind with
      | UnaryIdentity -> operand
      | UnaryNegation -> Int (-get_int operand))

let evaluate root = evaluate_expression root
