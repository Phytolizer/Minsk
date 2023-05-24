type t =
  | Parenthesized of parenthesized
  | Literal of literal
  | Binary of binary
  | Unary of unary

and parenthesized = {
  parenthesized_open_parenthesis_token : Token.t;
  parenthesized_expression : t;
  parenthesized_close_parenthesis_token : Token.t;
}

and literal = {
  literal_literal_token : Token.t;
  literal_value : Runtime.Value.t option;
}

and binary = {
  binary_left : t;
  binary_operator_token : Token.t;
  binary_right : t;
}

and unary = { unary_operator_token : Token.t; unary_operand : t }

type kind = KindBinary | KindParenthesized | KindLiteral | KindUnary
[@@deriving show { with_path = false }]

let kind x =
  match x with
  | Parenthesized _ -> KindParenthesized
  | Literal _ -> KindLiteral
  | Binary _ -> KindBinary
  | Unary _ -> KindUnary

module Parenthesized = struct
  type t = parenthesized

  let make open_parenthesis_token expression close_parenthesis_token =
    {
      parenthesized_open_parenthesis_token = open_parenthesis_token;
      parenthesized_expression = expression;
      parenthesized_close_parenthesis_token = close_parenthesis_token;
    }

  let open_parenthesis_token x = x.parenthesized_open_parenthesis_token
  let expression x = x.parenthesized_expression
  let close_parenthesis_token x = x.parenthesized_close_parenthesis_token
end

module Literal = struct
  type t = literal

  let make literal_literal_token literal_value =
    { literal_literal_token; literal_value }

  let literal_token x = x.literal_literal_token
  let value x = x.literal_value
end

module Binary = struct
  type t = binary

  let make left operator_token right =
    {
      binary_left = left;
      binary_operator_token = operator_token;
      binary_right = right;
    }

  let left x = x.binary_left
  let operator_token x = x.binary_operator_token
  let right x = x.binary_right
end

module Unary = struct
  type t = unary

  let make operator_token operand =
    { unary_operator_token = operator_token; unary_operand = operand }

  let operator_token x = x.unary_operator_token
  let operand x = x.unary_operand
end

let children ~xctor ~tctor x =
  match x with
  | Parenthesized p ->
      [|
        Parenthesized.open_parenthesis_token p |> tctor;
        Parenthesized.expression p |> xctor;
        Parenthesized.close_parenthesis_token p |> tctor;
      |]
  | Literal l -> [| Literal.literal_token l |> tctor |]
  | Binary b ->
      [|
        Binary.left b |> xctor;
        Binary.operator_token b |> tctor;
        Binary.right b |> xctor;
      |]
  | Unary u -> [| Unary.operator_token u |> tctor; Unary.operand u |> xctor |]
