open Token

let binary_operator_precedence = function
  | Star | Slash -> 4
  | Plus | Minus -> 3
  | AmpersandAmpersand -> 2
  | PipePipe -> 1
  | _ -> 0

let unary_operator_precedence = function Plus | Minus -> 5 | _ -> 0

let keyword_kind = function
  | "true" -> TrueKeyword
  | "false" -> FalseKeyword
  | _ -> Identifier
