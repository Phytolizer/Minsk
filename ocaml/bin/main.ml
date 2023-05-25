open Minsk.Runtime
open Minsk.Code_analysis

let () =
  let show_tree = ref false in
  let vars = Hashtbl.create 0 in
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
          let result = Compilation.evaluate vars syntax_tree in
          if !show_tree then (
            prerr_string "\x1b[2m";
            Syntax.Node.pretty_print stderr (Expression syntax_tree.root);
            prerr_string "\x1b[0m";
            flush stderr);
          match result with
          | Ok value ->
              Value.show stdout value;
              print_char '\n';
              flush stdout
          | Error diagnostics ->
              prerr_string "\x1b[0;31m";
              Array.iter
                (fun (d : Diagnostic.t) -> prerr_endline d.message)
                diagnostics;
              prerr_string "\x1b[0m";
              flush stderr)
    done
  with End_of_file -> prerr_endline ""
