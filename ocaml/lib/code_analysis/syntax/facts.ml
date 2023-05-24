open Token

let binary_operator_precedence = function
  | Star | Slash -> 2
  | Plus | Minus -> 1
  | _ -> 0

let unary_operator_precedence = function Plus | Minus -> 3 | _ -> 0
