type t =
  | Assignment of assignment
  | Binary of binary
  | Literal of literal
  | Name of name
  | Parenthesized of parenthesized
  | Unary of unary

and assignment = {
  assignment_identifier_token : Token.t;
  assignment_equals_token : Token.t;
  assignment_expression : t;
}

and binary = {
  binary_left : t;
  binary_operator_token : Token.t;
  binary_right : t;
}

and literal = {
  literal_literal_token : Token.t;
  literal_value : Runtime.Value.t option;
}

and name = { name_identifier_token : Token.t }

and parenthesized = {
  parenthesized_open_parenthesis_token : Token.t;
  parenthesized_expression : t;
  parenthesized_close_parenthesis_token : Token.t;
}

and unary = { unary_operator_token : Token.t; unary_operand : t }

type kind =
  | KindAssignment
  | KindBinary
  | KindLiteral
  | KindName
  | KindParenthesized
  | KindUnary
[@@deriving show { with_path = false }]
