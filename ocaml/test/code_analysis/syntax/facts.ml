open Alcotest
open Minsk.Code_analysis
module Syntax = Minsk.Code_analysis.Syntax

let get_text_round_trip kind text =
  let fmt = Printf.sprintf "%s: %s" (Token.show_kind kind) in
  let tokens = Syntax.Tree.parse_tokens text |> Array.of_seq in
  Simple_token.check_tokens tokens [| Simple_token.make kind text |] fmt

let get_text_round_trips_test () =
  Token.kinds
  |> Seq.iter (fun kind ->
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
