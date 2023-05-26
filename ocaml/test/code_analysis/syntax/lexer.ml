open Alcotest
open Minsk.Code_analysis
module Syntax = Minsk.Code_analysis.Syntax

let st = Simple_token.make

let tokens =
  Array.concat
    [
      Seq.filter_map
        (fun kind ->
          Syntax.Facts.get_text kind |> Option.map (fun text -> st kind text))
        Token.kinds
      |> Array.of_seq;
      [|
        st Number "1"; st Number "123"; st Identifier "a"; st Identifier "abc";
      |];
    ]

let separators =
  [|
    st Whitespace " ";
    st Whitespace "  ";
    st Whitespace "\r";
    st Whitespace "\n";
    st Whitespace "\r\n";
  |]

let requires_separator t1_kind t2_kind =
  let t1_is_keyword =
    Token.show_kind t1_kind |> String.ends_with ~suffix:"Keyword"
  in
  let t2_is_keyword =
    Token.show_kind t2_kind |> String.ends_with ~suffix:"Keyword"
  in
  match (t1_kind, t2_kind, t1_is_keyword, t2_is_keyword) with
  | Identifier, Identifier, _, _
  | Identifier, _, _, true
  | _, Identifier, true, _
  | _, _, true, true
  | (Number | Identifier), Number, _, _
  | _, Number, true, _
  | Bang, (Equals | EqualsEquals), _, _
  | Equals, (Equals | EqualsEquals), _, _ ->
      true
  | _ -> false

let get_tokens () =
  [ tokens; separators ] |> List.map Array.to_seq |> List.to_seq |> Seq.concat

let get_token_pairs () =
  let tokens = Array.to_seq tokens in
  tokens |> Seq.product tokens
  |> Seq.filter (fun ((t1 : Simple_token.t), (t2 : Simple_token.t)) ->
         not @@ requires_separator t1.kind t2.kind)

let get_token_pairs_with_separator () =
  let tokens = Array.to_seq tokens in
  tokens |> Seq.product tokens
  |> Seq.filter (fun ((t1 : Simple_token.t), (t2 : Simple_token.t)) ->
         requires_separator t1.kind t2.kind)
  |> Seq.product (Array.to_seq separators)
  |> Seq.map (fun (sep, (t1, t2)) -> (t1, sep, t2))

let lexes_token kind text =
  let fmt = Printf.sprintf "%s: %s" (Simple_token.show (st kind text)) in
  let tokens = Syntax.Tree.parse_tokens text |> Array.of_seq in
  Simple_token.check_tokens tokens [| st kind text |] fmt

let lexes_token_test () =
  get_tokens ()
  |> Seq.iter (fun ({ kind; text } : Simple_token.t) -> lexes_token kind text)

let lexes_token_pair (t1 : Simple_token.t) (t2 : Simple_token.t) =
  let fmt =
    Printf.sprintf "%s, %s: %s" (Simple_token.show t1) (Simple_token.show t2)
  in
  let text = String.concat "" [ t1.text; t2.text ] in
  let tokens = Syntax.Tree.parse_tokens text |> Array.of_seq in
  Simple_token.check_tokens tokens [| t1; t2 |] fmt

let lexes_token_pair_test () =
  get_token_pairs () |> Seq.iter (fun (t1, t2) -> lexes_token_pair t1 t2)

let lexes_token_pair_with_separator (t1 : Simple_token.t) (sep : Simple_token.t)
    (t2 : Simple_token.t) =
  let fmt =
    Printf.sprintf "%s, %s, %s: %s" (Simple_token.show t1)
      (Simple_token.show sep) (Simple_token.show t2)
  in
  let text = String.concat "" [ t1.text; sep.text; t2.text ] in
  let tokens = Syntax.Tree.parse_tokens text |> Array.of_seq in
  Simple_token.check_tokens tokens [| t1; sep; t2 |] fmt

let lexes_token_pair_with_separator_test () =
  get_token_pairs_with_separator ()
  |> Seq.iter (fun (t1, sep, t2) -> lexes_token_pair_with_separator t1 sep t2)

let suite () =
  run "Lexer"
    [
      ( "lexer",
        [
          test_case "lexes token" `Quick lexes_token_test;
          test_case "lexes token pair" `Slow lexes_token_pair_test;
          test_case "lexes token pair with separator" `Slow
            lexes_token_pair_with_separator_test;
        ] );
    ]
    ~and_exit:false
