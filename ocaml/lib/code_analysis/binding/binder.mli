open Runtime

val bind :
  Variable_map.t ->
  Syntax.Node.Expression.t ->
  Node.Expression.t * Diagnostic.t array
