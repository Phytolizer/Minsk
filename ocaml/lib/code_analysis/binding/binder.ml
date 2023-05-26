open Runtime
open Node.Expression
open Token
module S = Syntax.Node.Expression

type t = { variables : Variable_map.t; diagnostics : Diagnostic_bag.t }

let make variables = { variables; diagnostics = Diagnostic_bag.make () }

let rec bind_expression node b =
  match node with
  | S.Assignment x -> bind_assignment_expression x b
  | S.Binary x -> bind_binary_expression x b
  | S.Literal x -> bind_literal_expression x b
  | S.Parenthesized x -> bind_parenthesized_expression x b
  | S.Name x -> bind_name_expression x b
  | S.Unary x -> bind_unary_expression x b

and bind_assignment_expression node b =
  let name = node.assignment_identifier_token.text in
  let expression = bind_expression node.assignment_expression b in
  (match
     Hashtbl.to_seq_keys b.variables
     |> Seq.find (fun (k : Variable_symbol.t) -> k.name = name)
   with
  | Some v -> Hashtbl.remove b.variables v
  | None -> ());
  let v = Variable_symbol.make name (ty expression) in
  Hashtbl.add b.variables v (Int 0);
  Assignment (Assignment.make v expression)

and bind_binary_expression node b =
  let left = bind_expression (S.Binary.left node) b in
  let right = bind_expression (S.Binary.right node) b in
  match
    Binary.bind_op (S.Binary.operator_token node).kind (ty left) (ty right)
  with
  | Some op -> Binary (Binary.make left op right)
  | None ->
      let operator_token = S.Binary.operator_token node in
      Diagnostic_bag.report_undefined_binary_operator (span operator_token)
        ~operator_text:operator_token.text ~left_ty:(ty left)
        ~right_ty:(ty right) b.diagnostics;
      left

and bind_literal_expression node _ =
  node |> S.Literal.value |> Option.value ~default:(Value.Int 0) |> Literal.make
  |> fun x -> Literal x

and bind_name_expression node b =
  let name = node.name_identifier_token.text in
  match
    Hashtbl.to_seq_keys b.variables
    |> Seq.find (fun (k : Variable_symbol.t) -> k.name = name)
  with
  | None ->
      Diagnostic_bag.report_undefined_name
        (node.name_identifier_token |> span)
        ~name b.diagnostics;
      Literal (Literal.make @@ Value.Int 0)
  | Some value -> Variable (Variable.make value)

and bind_parenthesized_expression x =
  bind_expression (S.Parenthesized.expression x)

and bind_unary_expression x b =
  let operand = bind_expression (S.Unary.operand x) b in
  match Unary.bind_op (S.Unary.operator_token x).kind (ty operand) with
  | Some op -> Unary (Unary.make op operand)
  | None ->
      let operator_token = S.Unary.operator_token x in
      Diagnostic_bag.report_undefined_unary_operator (span operator_token)
        ~operator_text:operator_token.text ~operand_ty:(ty operand)
        b.diagnostics;
      operand

let bind vars node =
  let b = make vars in
  let node = bind_expression node b in
  (node, b.diagnostics |> Diagnostic_bag.to_array)
