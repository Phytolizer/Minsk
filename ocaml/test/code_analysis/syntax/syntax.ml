module Facts = Facts
module Lexer = Lexer

let suite () =
  Lexer.suite ();
  Facts.suite ()
