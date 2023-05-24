type t = {
  text : string;
  mutable position : int;
  mutable diagnostics : string BatVect.t;
}

let is_digit = function '0' .. '9' -> true | _ -> false
let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false
let make text = { text; position = 0; diagnostics = BatVect.empty }

let peek n l =
  let i = l.position + n in
  if i >= String.length l.text then None else Some l.text.[i]

let current = peek 0
let next l = l.position <- l.position + 1
let opt_and pred = function Some x -> pred x | None -> false
let curtext start l = String.sub l.text start (l.position - start)

let next_token l =
  let start = l.position in
  let kind = ref Token.Bad in
  let text = ref (None : string option) in
  let value = ref None in

  (match current l with
  | None -> kind := EndOfFile
  | Some c when is_digit c ->
      while current l |> opt_and is_digit do
        next l
      done;
      let ct = curtext start l in
      text := Some ct;
      kind := Number;
      value :=
        Some
          (match ct |> int_of_string_opt with
          | Some i -> i
          | None ->
              l.diagnostics <-
                BatVect.append
                  (Printf.sprintf "The number %s isn't a valid int." ct)
                  l.diagnostics;
              0)
  | Some c when is_space c ->
      while current l |> opt_and is_space do
        next l
      done;
      kind := Whitespace
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
  | Some _ -> next l);

  let text = match !text with Some t -> t | None -> curtext start l in
  Token.make !kind start text !value

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
