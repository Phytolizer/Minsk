open Alcotest
open Minsk.Code_analysis.Syntax.Node

let kind =
  let pp_kind ppf x = Fmt.pf ppf "%s" (show_kind x) in
  testable pp_kind ( = )
