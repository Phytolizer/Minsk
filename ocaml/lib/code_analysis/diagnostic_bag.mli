open Runtime

type t

val make : unit -> t
val add_range : t -> t -> unit
val to_array : t -> Diagnostic.t array

val report_invalid_number :
  Text.Span.t -> text:string -> ty:Value.ty -> t -> unit

val report_bad_character : position:int -> character:char -> t -> unit

val report_unexpected_token :
  Text.Span.t -> actual_kind:Token.kind -> expected_kind:Token.kind -> t -> unit

val report_undefined_unary_operator :
  Text.Span.t -> operator_text:string -> operand_ty:Value.ty -> t -> unit

val report_undefined_binary_operator :
  Text.Span.t ->
  operator_text:string ->
  left_ty:Value.ty ->
  right_ty:Value.ty ->
  t ->
  unit
