type t = {
  diagnostics : Diagnostic.t array;
  root : Node.Expression.t;
  end_of_file_token : Token.t;
}

val parse : string -> t
val parse_tokens : string -> Token.t Seq.t
