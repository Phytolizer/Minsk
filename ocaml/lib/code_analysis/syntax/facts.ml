open Token

let binary_operator_precedence = function
  | Star | Slash -> 5
  | Plus | Minus -> 4
  | EqualsEquals | BangEquals -> 3
  | AmpersandAmpersand -> 2
  | PipePipe -> 1
  | _ -> 0

let unary_operator_precedence = function Plus | Minus | Bang -> 6 | _ -> 0

let keyword_kind = function
  | "true" -> TrueKeyword
  | "false" -> FalseKeyword
  | _ -> Identifier
