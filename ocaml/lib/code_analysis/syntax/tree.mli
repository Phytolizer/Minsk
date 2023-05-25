type t = {
  diagnostics : Diagnostic.t array;
  root : Node.Expression.t;
  end_of_file_token : Token.t;
}

val parse : string -> t
