open Runtime

type t = {
  text : string;
  mutable position : int;
  diagnostics : Diagnostic_bag.t;
}

let is_digit = function '0' .. '9' -> true | _ -> false
let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let make text = { text; position = 0; diagnostics = Diagnostic_bag.make () }

let peek n l =
  let i = l.position + n in
  if i >= String.length l.text then None else Some l.text.[i]

let current = peek 0
let next l = l.position <- l.position + 1
let opt_and pred = function Some x -> pred x | None -> false

let rec orp xs y =
  match xs with x :: xs -> if x y then true else orp xs y | [] -> false

let curtext start l = String.sub l.text start (l.position - start)

let next_token l =
  let start = l.position in
  let kind = ref Token.Bad in
  let text = lazy (curtext start l) in
  let value = ref None in

  (match current l with
  | None -> kind := EndOfFile
  | Some c when is_digit c ->
      while current l |> opt_and is_digit do
        next l
      done;
      kind := Number;
      value :=
        Some
          (match Lazy.force text |> int_of_string_opt with
          | Some i -> Value.Int i
          | None ->
              Diagnostic_bag.report_invalid_number
                (Text.Span.make start (Lazy.force text |> String.length))
                ~text:(Lazy.force text) ~ty:Value.TyInt l.diagnostics;
              Value.Int 0)
  | Some c when is_space c ->
      while current l |> opt_and is_space do
        next l
      done;
      kind := Whitespace
  | Some c when is_letter c ->
      while current l |> opt_and (orp [ is_letter; is_digit ]) do
        next l
      done;
      kind := Lazy.force text |> Facts.keyword_kind
  | Some '+' ->
      next l;
      kind := Plus
  | Some '-' ->
      next l;
      kind := Minus
  | Some '*' ->
      next l;
      kind := Star
  | Some '/' ->
      next l;
      kind := Slash
  | Some '(' ->
      next l;
      kind := OpenParenthesis
  | Some ')' ->
      next l;
      kind := CloseParenthesis
  | Some '=' when peek 1 l = Some '=' ->
      next l;
      next l;
      kind := EqualsEquals
  | Some '=' ->
      next l;
      kind := Equals
  | Some '!' when peek 1 l = Some '=' ->
      next l;
      next l;
      kind := BangEquals
  | Some '!' ->
      next l;
      kind := Bang
  | Some '&' when peek 1 l = Some '&' ->
      next l;
      next l;
      kind := AmpersandAmpersand
  | Some '|' when peek 1 l = Some '|' ->
      next l;
      next l;
      kind := PipePipe
  | Some _ -> next l);

  Token.make !kind start (Lazy.force text) !value

let lex text =
  let l = make text in
  let tokens = ref (BatVect.empty : Token.t BatVect.t) in
  let looping = ref true in
  while !looping do
    let t = next_token l in
    (match t.kind with Token.EndOfFile -> looping := false | _ -> ());
    tokens := BatVect.append t !tokens
  done;
  (BatVect.to_array !tokens, l.diagnostics)
