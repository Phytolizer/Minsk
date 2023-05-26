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

let get_text = function
  | Plus -> Some "+"
  | Minus -> Some "-"
  | Star -> Some "*"
  | Slash -> Some "/"
  | Bang -> Some "!"
  | Equals -> Some "="
  | AmpersandAmpersand -> Some "&&"
  | PipePipe -> Some "||"
  | EqualsEquals -> Some "=="
  | BangEquals -> Some "!="
  | OpenParenthesis -> Some "("
  | CloseParenthesis -> Some ")"
  | FalseKeyword -> Some "false"
  | TrueKeyword -> Some "true"
  | _ -> None

let unary_operator_kinds =
  Token.kinds |> Seq.filter (fun kind -> unary_operator_precedence kind > 0)

let binary_operator_kinds =
  Token.kinds |> Seq.filter (fun kind -> binary_operator_precedence kind > 0)
