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
[@@deriving show, enum]

type t = {
  kind : kind;
  position : int;
  text : string;
  value : Runtime.Value.t option;
}


val make : kind -> int -> string -> Runtime.Value.t option -> t
val span : t -> Text.Span.t
val kinds : kind Seq.t
