module Evaluator = Evaluator
module Syntax = Syntax

let suite () =
  Syntax.suite ();
  Evaluator.suite ()
