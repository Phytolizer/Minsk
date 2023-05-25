open Runtime
open Types

type t = unary
type op = unary_op

val make : op -> expr -> t
val bind_op : Syntax.Token.kind -> Value.ty -> op option
val op : t -> op
val operand : t -> expr

module Op : sig
  val kind : op -> unary_operator_kind
  val result_ty : op -> Value.ty
end
