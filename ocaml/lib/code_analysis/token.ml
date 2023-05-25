open Runtime
open Text

type kind =
  | Number
  | Identifier
  | Whitespace
  | Plus
  | Minus
  | Star
  | Slash
  | Bang
  | Equals
  | AmpersandAmpersand
  | PipePipe
  | EqualsEquals
  | BangEquals
  | OpenParenthesis
  | CloseParenthesis
  | TrueKeyword
  | FalseKeyword
  | Bad
  | EndOfFile
[@@deriving show]

type t = { kind : kind; position : int; text : string; value : Value.t option }

let make kind position text value = { kind; position; text; value }
let span x = Span.make x.position (String.length x.text)
