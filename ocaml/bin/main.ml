open Minsk.Runtime
open Minsk.Code_analysis
open Minsk.Code_analysis.Binding

let () =
  let show_tree = ref false in
  try
    while true do
      prerr_string "> ";
      flush stderr;
      let line = read_line () in
      match line with
      | "#showTree" ->
          show_tree := not !show_tree;
          (if !show_tree then "Showing parse trees."
           else "Not showing parse trees.")
          |> print_endline
      | "#cls" ->
          print_string "\x1b[2J\x1b[H";
          flush stdout
      | line -> (
          let syntax_tree = Syntax.Tree.parse line in
          let bound_expression, diagnostics = Binder.bind syntax_tree.root in
          let diagnostics =
            Array.concat [ syntax_tree.diagnostics; diagnostics ]
          in
          if !show_tree then (
            prerr_string "\x1b[2m";
            Syntax.Node.pretty_print stderr (Expression syntax_tree.root);
            prerr_string "\x1b[0m";
            flush stderr);
          match diagnostics with
          | [||] ->
              let result = Evaluator.evaluate bound_expression in
              Value.show stdout result;
              print_char '\n';
              flush stdout
          | _ ->
              prerr_string "\x1b[0;31m";
              Array.iter prerr_endline diagnostics;
              prerr_string "\x1b[0m";
              flush stderr)
    done
  with End_of_file -> prerr_endline ""
