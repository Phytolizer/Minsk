open Runtime

let sprintf = Printf.sprintf

type t = { mutable diagnostics : Diagnostic.t BatVect.t }

let make () = { diagnostics = BatVect.empty }
let add_range x y = x.diagnostics <- BatVect.concat x.diagnostics y.diagnostics
let to_array x = BatVect.to_array x.diagnostics

let report span message x =
  x.diagnostics <- BatVect.append (Diagnostic.make span message) x.diagnostics

let report_invalid_number span ~text ~ty =
  let message =
    sprintf "The number %s isn't a valid %s." text (Value.show_ty ty)
  in
  report span message

let report_bad_character ~position ~character =
  let span = Text.Span.make position 1 in
  let message = sprintf "Bad character in input: '%c'" character in
  report span message

let report_unexpected_token span ~actual_kind ~expected_kind =
  let message =
    sprintf "Unexpected token <%s>, expected <%s>."
      (Token.show_kind actual_kind)
      (Token.show_kind expected_kind)
  in
  report span message

let report_undefined_unary_operator span ~operator_text ~operand_ty =
  let message =
    sprintf "The unary operator '%s' is not defined for type %s." operator_text
      (Value.show_ty operand_ty)
  in
  report span message

let report_undefined_binary_operator span ~operator_text ~left_ty ~right_ty =
  let message =
    sprintf "The binary operator '%s' is not defined for types %s and %s."
      operator_text (Value.show_ty left_ty) (Value.show_ty right_ty)
  in
  report span message

let report_undefined_name span ~name =
  let message = sprintf "Variable '%s' doesn't exist." name in
  report span message
