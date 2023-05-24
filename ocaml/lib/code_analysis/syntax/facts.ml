open Token

let binary_operator_precedence kind =
  match kind with Star | Slash -> 2 | Plus | Minus -> 1 | _ -> 0
