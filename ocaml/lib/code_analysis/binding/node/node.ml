module Expression = Expression

type t = Expression of Expression.t
type kind = KindExpression

let kind x = match x with Expression e -> Expression.kind e
