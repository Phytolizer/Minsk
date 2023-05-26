open Alcotest
open Minsk.Code_analysis
module Syntax = Minsk.Code_analysis.Syntax

let get_text_round_trip kind text =
  let fmt = Printf.sprintf "%s: %s" (Token.show_kind kind) in
  let tokens = Syntax.Tree.parse_tokens text |> Array.of_seq in
  Simple_token.check_tokens tokens [| Simple_token.make kind text |] fmt

let range low high =
  let rec g i = if i <= high then Seq.cons i (g @@ (i + 1)) else Seq.empty in
  g low

let get_text_round_trips_test () =
  range Token.min_kind Token.max_kind
  |> Seq.iter (fun i ->
         let kind = Token.kind_of_enum i |> Option.get in
         match Syntax.Facts.get_text kind with
         | None -> ()
         | Some text -> get_text_round_trip kind text)

let suite () =
  run "Facts"
    [
      ( "facts",
        [ test_case "get text round trips" `Quick get_text_round_trips_test ] );
    ]
    ~and_exit:false
