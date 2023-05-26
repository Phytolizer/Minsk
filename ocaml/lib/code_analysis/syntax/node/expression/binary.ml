open Types

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
