open Minsk.Runtime
open Minsk.Code_analysis

let red () = prerr_string "\x1b[0;31m"
let reset () = prerr_string "\x1b[0m"

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
              print_string @@ Value.show value;
              print_char '\n';
              flush stdout
          | Error diagnostics ->
              let text = syntax_tree.text in
              Array.iter
                (fun (d : Diagnostic.t) ->
                  let line_index = Text.Source.line_index text d.span.start in
                  let line_number = line_index + 1 in
                  let character =
                    d.span.start - (Text.Source.line text line_index).start + 1
                  in
                  red ();
                  Printf.eprintf "(%d, %d): " line_number character;
                  prerr_endline d.message;
                  reset ())
                diagnostics;
              flush stderr)
    done
  with End_of_file -> prerr_endline ""
