module Expression = Expression

type kind = KindToken | KindExpression of Expression.kind [@@deriving show]
type t = Expression of Expression.t | Token of Token.t

val kind : t -> kind
val children : t -> t array
val pretty_print : out_channel -> t -> unit
val span : t -> Text.Span.t
