open Runtime
open Node.Expression
open Syntax.Token
module S = Syntax.Node.Expression

type t = { mutable diagnostics : string BatVect.t }

let make () = { diagnostics = BatVect.empty }

let bind_binary_operator_kind kind left right =
  match (left, right) with
  | Value.TyInt, Value.TyInt -> (
      match kind with
      | Plus -> Some Addition
      | Minus -> Some Subtraction
      | Star -> Some Multiplication
      | Slash -> Some Division
      | _ -> None)
  | Value.TyBool, Value.TyBool -> (
      match kind with
      | AmpersandAmpersand -> Some LogicalAnd
      | PipePipe -> Some LogicalOr
      | _ -> None)
  | _ -> None

let bind_unary_operator_kind kind operand =
  match operand with
  | Value.TyInt -> (
      match kind with
      | Plus -> Some Identity
      | Minus -> Some Negation
      | _ -> None)
  | Value.TyBool -> (
      match kind with Bang -> Some LogicalNegation | _ -> None)

let rec bind_expression node b =
  match node with
  | S.Binary x -> bind_binary_expression x b
  | Literal x -> bind_literal_expression x b
  | Parenthesized x -> bind_parenthesized_expression x b
  | Unary x -> bind_unary_expression x b

and bind_binary_expression node b =
  let left = bind_expression (S.Binary.left node) b in
  let right = bind_expression (S.Binary.right node) b in
  match
    bind_binary_operator_kind (S.Binary.operator_token node).kind (ty left)
      (ty right)
  with
  | Some operator_kind -> Binary (Binary.make left operator_kind right)
  | None ->
      b.diagnostics <-
        BatVect.append
          (Printf.sprintf
             "Binary operator '%s' is not defined for types %s and %s."
             (S.Binary.operator_token node).text
             (Value.show_ty (ty left))
             (Value.show_ty (ty right)))
          b.diagnostics;
      left

and bind_literal_expression node _ =
  node |> S.Literal.value |> Option.value ~default:(Value.Int 0) |> Literal.make
  |> fun x -> Literal x

and bind_parenthesized_expression x =
  bind_expression (S.Parenthesized.expression x)

and bind_unary_expression x b =
  let operand = bind_expression (S.Unary.operand x) b in
  match
    bind_unary_operator_kind (S.Unary.operator_token x).kind (ty operand)
  with
  | Some operator_kind -> Unary (Unary.make operator_kind operand)
  | None -> operand

let bind node =
  let b = make () in
  let node = bind_expression node b in
  (node, b.diagnostics |> BatVect.to_array)
