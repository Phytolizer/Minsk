open Types

type t = unary

let make operator_token operand =
  { unary_operator_token = operator_token; unary_operand = operand }

let operator_token x = x.unary_operator_token
let operand x = x.unary_operand
