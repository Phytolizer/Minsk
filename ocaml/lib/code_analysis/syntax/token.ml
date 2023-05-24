type kind =
  | Number
  | Whitespace
  | Plus
  | Minus
  | Star
  | Slash
  | OpenParenthesis
  | CloseParenthesis
  | Bad
  | EndOfFile
[@@deriving show]

type value = int option
type t = { kind : kind; position : int; text : string; value : value }

let make kind position text value = { kind; position; text; value }
