open Minsk.Code_analysis

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
          if !show_tree then (
            prerr_string "\x1b[2m";
            Syntax.Node.pretty_print stderr (Expression syntax_tree.root);
            prerr_string "\x1b[0m";
            flush stderr);
          match syntax_tree.diagnostics with
          | [||] ->
              let result = Evaluator.evaluate syntax_tree.root in
              Printf.printf "%d\n" result;
              flush stdout
          | diagnostics ->
              prerr_string "\x1b[1;31m";
              Array.iter prerr_endline diagnostics;
              prerr_string "\x1b[0m";
              flush stderr)
    done
  with End_of_file -> prerr_endline ""
