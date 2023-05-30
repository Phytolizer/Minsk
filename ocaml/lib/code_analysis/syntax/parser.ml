open Node.Expression
open Token
open Facts

type t = {
  tokens : Token.t array;
  diagnostics : Diagnostic_bag.t;
  mutable position : int;
}

let make text =
  let tokens, diagnostics = Lexer.lex text in
  let tokens =
    Array.to_seq tokens
    |> Seq.filter (fun (t : Token.t) ->
           match t.kind with Bad | Whitespace -> false | _ -> true)
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

let match_fail actual_kind expected_kind p =
  Diagnostic_bag.report_unexpected_token
    (current p |> Token.span)
    ~actual_kind ~expected_kind p.diagnostics;
  Token.make expected_kind (current p).position "" None

let match_token kind p =
  match (current p).kind with
  | k when k = kind -> next_token p
  | k -> match_fail k kind p

let rec parse_expression p = parse_assignment_expression p

and parse_assignment_expression p =
  if (peek 0 p).kind = Identifier && (peek 1 p).kind = Equals then
    let identifier_token = next_token p in
    let operator_token = next_token p in
    let right = parse_assignment_expression p in
    Assignment (Assignment.make identifier_token operator_token right)
  else parse_binary_expression ~parent_precedence:0 p

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
  | OpenParenthesis -> parse_parenthesized_expression p
  | FalseKeyword | TrueKeyword -> parse_boolean_literal p
  | Number -> parse_number_literal p
  | _ -> parse_name_expression p

and parse_parenthesized_expression p =
  let left = match_token OpenParenthesis p in
  let expression = parse_expression p in
  let right = match_token CloseParenthesis p in
  Parenthesized (Parenthesized.make left expression right)

and parse_boolean_literal p =
  let is_true = (current p).kind = TrueKeyword in
  let keyword_token =
    match_token (if is_true then TrueKeyword else FalseKeyword) p
  in
  Literal (Literal.make keyword_token (Some (Bool is_true)))

and parse_number_literal p =
  let number_token = match_token Number p in
  Literal (Literal.make number_token number_token.value)

and parse_name_expression p =
  let identifier_token = match_token Identifier p in
  Name (Name.make identifier_token)

let parse text =
  let p = make text in
  let expr = parse_expression p in
  let end_of_file_token = match_token EndOfFile p in
  (p.diagnostics |> Diagnostic_bag.to_array, expr, end_of_file_token)
