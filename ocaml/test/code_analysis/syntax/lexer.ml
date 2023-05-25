open Alcotest

let works () = (check bool) "works" true true
let suite () = run "Lexer" [ ("lexer", [ test_case "works" `Quick works ]) ]
