open Syntax.Token
open Syntax.Node.Expression

let rec evaluate_expression node =
  match node with
  | Literal l -> l.literal_literal_token.value |> Option.get
  | Binary b -> (
      let left = evaluate_expression b.binary_left in
      let right = evaluate_expression b.binary_right in
      match b.binary_operator_token.kind with
      | Plus -> left + right
      | Minus -> left - right
      | Star -> left * right
      | Slash -> left / right
      | _ -> failwith "unreachable")
  | Parenthesized p -> evaluate_expression p.parenthesized_expression

let evaluate root = evaluate_expression root
