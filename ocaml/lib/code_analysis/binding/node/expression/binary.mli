open Runtime
open Types

type t = binary
type op = binary_op

val make : expr -> op -> expr -> t
val bind_op : Token.kind -> Value.ty -> Value.ty -> op option
val left : t -> expr
val op : t -> op
val right : t -> expr

module Op : sig
  val kind : op -> binary_operator_kind
  val result_ty : op -> Value.ty
end
