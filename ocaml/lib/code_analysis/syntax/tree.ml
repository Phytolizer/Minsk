type t = {
  diagnostics : Diagnostic.t array;
  root : Node.Expression.t;
  end_of_file_token : Token.t;
}

let parse text =
  let diagnostics, root, end_of_file_token = Parser.parse text in
  { diagnostics; root; end_of_file_token }

let parse_tokens text =
  let tokens, _ = Lexer.lex text in
  Array.to_seq tokens
  |> Seq.take_while (fun (tok : Token.t) -> tok.kind != Token.EndOfFile)
