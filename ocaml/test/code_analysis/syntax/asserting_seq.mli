open Minsk.Code_analysis
module Syntax = Minsk.Code_analysis.Syntax

type t

val make : Syntax.Node.t -> t
val assert_node : fmt:(string -> string) -> kind:Syntax.Node.kind -> t -> unit

val assert_token :
  fmt:(string -> string) -> kind:Token.kind -> text:string -> t -> unit

val finish : fmt:(string -> string) -> t -> unit
