type t = {
  diagnostics : string array;
  root : Node.Expression.t;
  end_of_file_token : Token.t;
}

val parse : string -> t
