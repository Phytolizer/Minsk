type t

val make : string -> t
val ref : t -> int -> char
val sub : t -> int -> int -> string
val sub_span : t -> Span.t -> string
val length : t -> int
val line : t -> int -> Line.t
val line_index : t -> int -> int
