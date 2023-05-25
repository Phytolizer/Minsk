open Runtime.Value
open Binding.Node.Expression

let rec evaluate_expression vars node =
  match node with
  | Assignment a ->
      let value = evaluate_expression vars a.assignment_expression in
      Hashtbl.add vars a.assignment_variable value;
      value
  | Binary b -> (
      let left = evaluate_expression vars b.binary_left in
      let right = evaluate_expression vars b.binary_right in
      match b.binary_op.bop_kind with
      | Addition -> Int (get_int left + get_int right)
      | Subtraction -> Int (get_int left - get_int right)
      | Multiplication -> Int (get_int left * get_int right)
      | Division -> Int (get_int left / get_int right)
      | LogicalAnd -> Bool (get_bool left && get_bool right)
      | LogicalOr -> Bool (get_bool left || get_bool right)
      | Equality -> Bool (left = right)
      | Inequality -> Bool (left = right |> not))
  | Literal l -> l.literal_value
  | Unary u -> (
      let operand = evaluate_expression vars u.unary_operand in
      match u.unary_op.uop_kind with
      | Identity -> operand
      | Negation -> Int (-get_int operand)
      | LogicalNegation -> Bool (get_bool operand |> not))
  | Variable v -> Hashtbl.find vars v.variable_variable

let evaluate variables root = evaluate_expression variables root
