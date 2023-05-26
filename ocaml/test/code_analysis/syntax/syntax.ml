module Facts = Facts
module Lexer = Lexer
module Parser = Parser

let suite () =
  Lexer.suite ();
  Facts.suite ();
  Parser.suite ()
