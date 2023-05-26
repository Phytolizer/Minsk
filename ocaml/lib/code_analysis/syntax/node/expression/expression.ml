include Types
module Assignment = Assignment
module Binary = Binary
module Literal = Literal
module Name = Name
module Parenthesized = Parenthesized
module Unary = Unary

let kind x =
  match x with
  | Assignment _ -> KindAssignment
  | Binary _ -> KindBinary
  | Literal _ -> KindLiteral
  | Name _ -> KindName
  | Parenthesized _ -> KindParenthesized
  | Unary _ -> KindUnary

let children ~xctor ~tctor x =
  match x with
  | Assignment a ->
      [|
        Assignment.identifier_token a |> tctor;
        Assignment.equals_token a |> tctor;
        Assignment.expression a |> xctor;
      |]
  | Binary b ->
      [|
        Binary.left b |> xctor;
        Binary.operator_token b |> tctor;
        Binary.right b |> xctor;
      |]
  | Literal l -> [| Literal.literal_token l |> tctor |]
  | Name n -> [| Name.identifier_token n |> tctor |]
  | Parenthesized p ->
      [|
        Parenthesized.open_parenthesis_token p |> tctor;
        Parenthesized.expression p |> xctor;
        Parenthesized.close_parenthesis_token p |> tctor;
      |]
  | Unary u -> [| Unary.operator_token u |> tctor; Unary.operand u |> xctor |]
