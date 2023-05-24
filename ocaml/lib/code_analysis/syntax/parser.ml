open Node.Expression
open Facts

type t = {
  tokens : Token.t array;
  mutable diagnostics : string BatVect.t;
  mutable position : int;
}

let make text =
  let tokens, diagnostics = Lexer.lex text in
  let tokens =
    Array.to_seq tokens
    |> Seq.filter (fun (t : Token.t) ->
           match t.kind with Token.Bad | Whitespace -> false | _ -> true)
    |> Array.of_seq
  in
  { tokens; diagnostics; position = 0 }

let peek offset p =
  let i = p.position + offset in
  if i >= Array.length p.tokens then p.tokens.(Array.length p.tokens - 1)
  else p.tokens.(i)

let current = peek 0

let next_token p =
  let c = current p in
  p.position <- p.position + 1;
  c

let match_fail actual expected p =
  p.diagnostics <-
    BatVect.append
      (Printf.sprintf "ERROR: Unexpected token <%s>, expected <%s>"
         (Token.show_kind actual) (Token.show_kind expected))
      p.diagnostics;
  Token.make expected (current p).position "" None

let match_token kind p =
  match (current p).kind with
  | k when k = kind -> next_token p
  | k -> match_fail k kind p

let rec parse_expression p = parse_binary_expression ~parent_precedence:0 p

and parse_binary_expression ~parent_precedence p =
  let rec next left =
    let precedence = binary_operator_precedence (current p).kind in
    if precedence = 0 || precedence <= parent_precedence then left
    else
      let operator_token = next_token p in
      let right = parse_binary_expression ~parent_precedence:precedence p in
      Binary (Binary.make left operator_token right) |> next
  in
  let unary_precedence = unary_operator_precedence (current p).kind in
  (if unary_precedence = 0 || unary_precedence < parent_precedence then
     parse_primary_expression p
   else
     let operator_token = next_token p in
     let operand =
       parse_binary_expression ~parent_precedence:unary_precedence p
     in
     Unary (Unary.make operator_token operand))
  |> next

and parse_primary_expression p =
  match (current p).kind with
  | Token.OpenParenthesis ->
      let left = next_token p in
      let expression = parse_expression p in
      let right = match_token CloseParenthesis p in
      Parenthesized (Parenthesized.make left expression right)
  | _ ->
      let number_token = match_token Number p in
      Literal (Literal.make number_token)

let parse text =
  let p = make text in
  let expr = parse_expression p in
  let end_of_file_token = match_token EndOfFile p in
  (p.diagnostics |> BatVect.to_array, expr, end_of_file_token)