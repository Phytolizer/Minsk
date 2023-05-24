open Runtime

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

type t = { kind : kind; position : int; text : string; value : Value.t option }

let make kind position text value = { kind; position; text; value }
