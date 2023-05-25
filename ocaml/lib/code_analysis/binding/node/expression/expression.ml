open Runtime
include Types
module Assignment = Assignment
module Binary = Binary
module Literal = Literal
module Unary = Unary
module Variable = Variable

let rec ty x =
  match x with
  | Assignment a -> Assignment.expression a |> ty
  | Binary b -> Binary.op b |> Binary.Op.result_ty
  | Literal l -> Literal.value l |> Value.tyof
  | Unary u -> Unary.op u |> Unary.Op.result_ty
  | Variable v -> (Variable.variable v).ty
