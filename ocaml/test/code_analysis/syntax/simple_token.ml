open Alcotest
open Minsk.Code_analysis
module Syntax = Minsk.Code_analysis.Syntax

type t = { kind : Token.kind; text : string } [@@deriving show]

let make kind text = { kind; text }
let of_token (x : Token.t) = { kind = x.kind; text = x.text }

let token_kind =
  let pp_kind ppf x = Fmt.pf ppf "%s" (Token.show_kind x) in
  testable pp_kind ( = )

let token =
  let pp_token ppf (x : t) =
    Fmt.pf ppf "(%s '%s')" (Token.show_kind x.kind) x.text
  in
  testable pp_token ( = )

let check_tokens act exp fmt =
  (check int) (fmt "correct count") (Array.length exp) (Array.length act);
  Seq.iteri
    (fun i (t, a) ->
      (check token) (fmt (Printf.sprintf "correct token %d" i)) t a)
    (Seq.zip (act |> Array.to_seq |> Seq.map of_token) (exp |> Array.to_seq))
