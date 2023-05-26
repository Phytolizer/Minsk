open Alcotest
open Minsk.Code_analysis
module Value_ext = Runtime.Value_ext
open Minsk.Runtime
module Syntax = Minsk.Code_analysis.Syntax

let correct_evaluation text expected =
  let fmt = Printf.sprintf "'%s' -> %s: %s" text (Value.show expected) in
  match Syntax.Tree.parse text |> Compilation.evaluate (Hashtbl.create 0) with
  | Error e ->
      fail @@ fmt
      @@ Printf.sprintf "had errors: %s"
      @@ String.concat "" @@ Array.to_list
      @@ Array.map (fun (x : Diagnostic.t) -> x.message) e
  | Ok value -> (check Value_ext.value) (fmt "correct value") expected value

let correct_evaluation_test () =
  Array.iter
    (fun (text, expected) -> correct_evaluation text expected)
    [|
      ("1", Int 1);
      ("+1", Int 1);
      ("-1", Int (-1));
      ("14 + 12", Int 26);
      ("12 - 3", Int 9);
      ("4 * 2", Int 8);
      ("9 / 3", Int 3);
      ("(10)", Int 10);
      ("12 == 3", Bool false);
      ("3 == 3", Bool true);
      ("12 != 3", Bool true);
      ("3 != 3", Bool false);
      ("false == false", Bool true);
      ("true == false", Bool false);
      ("false != false", Bool false);
      ("true != false", Bool true);
      ("true", Bool true);
      ("false", Bool false);
      ("!true", Bool false);
      ("!false", Bool true);
      ("(a = 10) * a", Int 100);
    |]

let suite () =
  run "Evaluator"
    [
      ( "evaluator",
        [ test_case "correct evaluation" `Quick correct_evaluation_test ] );
    ]
