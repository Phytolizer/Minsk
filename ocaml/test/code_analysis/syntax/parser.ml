open Alcotest
open Asserting_seq
module Syntax = Minsk.Code_analysis.Syntax
module Expression = Syntax.Node.Expression

let binary = Syntax.Node.KindExpression Expression.KindBinary
let unary = Syntax.Node.KindExpression Expression.KindUnary
let name = Syntax.Node.KindExpression Expression.KindName

let parse text =
  Syntax.Tree.parse text |> fun x -> Syntax.Node.Expression x.root

let binary_expression_precedence op1 op2 =
  let op1_precedence = Syntax.Facts.binary_operator_precedence op1 in
  let op2_precedence = Syntax.Facts.binary_operator_precedence op2 in
  let op1_text = Syntax.Facts.get_text op1 |> Option.get in
  let op2_text = Syntax.Facts.get_text op2 |> Option.get in
  let fmt = Printf.sprintf "'%s', '%s': %s" op1_text op2_text in
  let text = Printf.sprintf "a %s b %s c" op1_text op2_text in
  let expression = parse text in

  if op1_precedence >= op2_precedence then (
    let e = Asserting_seq.make expression in
    assert_node ~fmt ~kind:binary e;
    assert_node ~fmt ~kind:binary e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"a" e;
    assert_token ~fmt ~kind:op1 ~text:op1_text e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"b" e;
    assert_token ~fmt ~kind:op2 ~text:op2_text e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"c" e;
    finish ~fmt e)
  else
    let e = Asserting_seq.make expression in
    assert_node ~fmt ~kind:binary e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"a" e;
    assert_token ~fmt ~kind:op1 ~text:op1_text e;
    assert_node ~fmt ~kind:binary e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"b" e;
    assert_token ~fmt ~kind:op2 ~text:op2_text e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"c" e;
    finish ~fmt e

let binary_operator_pairs =
  let ops = Syntax.Facts.binary_operator_kinds in
  Seq.product ops ops

let binary_expression_precedence_test () =
  Seq.iter
    (fun (op1, op2) -> binary_expression_precedence op1 op2)
    binary_operator_pairs

let unary_expression_precedence ukind bkind =
  let unary_precedence = Syntax.Facts.unary_operator_precedence ukind in
  let binary_precedence = Syntax.Facts.binary_operator_precedence bkind in
  let unary_text = Syntax.Facts.get_text ukind |> Option.get in
  let binary_text = Syntax.Facts.get_text bkind |> Option.get in
  let fmt = Printf.sprintf "'%s', '%s': %s" unary_text binary_text in
  let text = Printf.sprintf "%s a %s b" unary_text binary_text in
  let expression = parse text in

  if unary_precedence >= binary_precedence then (
    let e = Asserting_seq.make expression in
    assert_node ~fmt ~kind:binary e;
    assert_node ~fmt ~kind:unary e;
    assert_token ~fmt ~kind:ukind ~text:unary_text e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"a" e;
    assert_token ~fmt ~kind:bkind ~text:binary_text e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"b" e;
    finish ~fmt e)
  else
    let e = Asserting_seq.make expression in
    assert_node ~fmt ~kind:unary e;
    assert_token ~fmt ~kind:ukind ~text:unary_text e;
    assert_node ~fmt ~kind:binary e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"a" e;
    assert_token ~fmt ~kind:bkind ~text:binary_text e;
    assert_node ~fmt ~kind:name e;
    assert_token ~fmt ~kind:Identifier ~text:"b" e;
    finish ~fmt e

let unary_operator_pairs =
  Seq.product Syntax.Facts.unary_operator_kinds
    Syntax.Facts.binary_operator_kinds

let unary_expression_precedence_test () =
  Seq.iter
    (fun (ukind, bkind) -> unary_expression_precedence ukind bkind)
    unary_operator_pairs

let suite () =
  run "Parser"
    [
      ( "parser",
        [
          test_case "binary expression honors precedence" `Quick
            binary_expression_precedence_test;
          test_case "unary expression honors precedence" `Quick
            unary_expression_precedence_test;
        ] );
    ]
    ~and_exit:false
