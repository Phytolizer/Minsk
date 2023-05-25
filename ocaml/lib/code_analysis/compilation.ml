open Runtime

let evaluate (variables : Variable_map.t) (syntax_tree : Syntax.Tree.t) =
  let bound_expression, diagnostics =
    Binding.Binder.bind variables syntax_tree.root
  in
  let diagnostics = Array.concat [ syntax_tree.diagnostics; diagnostics ] in
  match diagnostics with
  | [||] ->
      let value = Evaluator.evaluate variables bound_expression in
      Ok value
  | diagnostics -> Error diagnostics
