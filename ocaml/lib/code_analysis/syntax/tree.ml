type t = {
  diagnostics : string array;
  root : Node.Expression.t;
  end_of_file_token : Token.t;
}

let parse text =
  let diagnostics, root, end_of_file_token = Parser.parse text in
  { diagnostics; root; end_of_file_token }
