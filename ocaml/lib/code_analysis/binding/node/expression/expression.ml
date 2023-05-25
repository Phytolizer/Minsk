open Runtime
include Types
module Binary = Binary
module Literal = Literal
module Unary = Unary

let ty x =
  match x with
  | Binary b -> Binary.op b |> Binary.Op.result_ty
  | Literal l -> Literal.value l |> Value.tyof
  | Unary u -> Unary.op u |> Unary.Op.result_ty
