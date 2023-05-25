open Runtime

type kind =
  | Number
  | Identifier
  | Whitespace
  | Plus
  | Minus
  | Star
  | Slash
  | Bang
  | AmpersandAmpersand
  | PipePipe
  | OpenParenthesis
  | CloseParenthesis
  | TrueKeyword
  | FalseKeyword
  | Bad
  | EndOfFile
[@@deriving show]

type t = { kind : kind; position : int; text : string; value : Value.t option }

let make kind position text value = { kind; position; text; value }
