open Runtime.Value
open Binding.Node.Expression

let rec evaluate_expression node =
  match node with
  | Literal l -> l.literal_value
  | Binary b -> (
      let left = evaluate_expression b.binary_left in
      let right = evaluate_expression b.binary_right in
      match b.binary_op.bop_kind with
      | Addition -> Int (get_int left + get_int right)
      | Subtraction -> Int (get_int left - get_int right)
      | Multiplication -> Int (get_int left * get_int right)
      | Division -> Int (get_int left / get_int right)
      | LogicalAnd -> Bool (get_bool left && get_bool right)
      | LogicalOr -> Bool (get_bool left || get_bool right)
      | Equality -> Bool (left = right)
      | Inequality -> Bool (left = right |> not))
  | Unary u -> (
      let operand = evaluate_expression u.unary_operand in
      match u.unary_op.uop_kind with
      | Identity -> operand
      | Negation -> Int (-get_int operand)
      | LogicalNegation -> Bool (get_bool operand |> not))

let evaluate root = evaluate_expression root
