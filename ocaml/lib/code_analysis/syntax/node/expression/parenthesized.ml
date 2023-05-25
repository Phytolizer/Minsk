open Types

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
