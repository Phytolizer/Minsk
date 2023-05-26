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
[@@deriving show, enum]

type t = { kind : kind; position : int; text : string; value : Value.t option }

let make kind position text value = { kind; position; text; value }
let span x = Span.make x.position (String.length x.text)

let range low high =
  let rec g i = if i <= high then Seq.cons i (g @@ (i + 1)) else Seq.empty in
  g low

let kinds =
  range min_kind max_kind |> Seq.map (fun i -> kind_of_enum i |> Option.get)
