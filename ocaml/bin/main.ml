open Minsk.Runtime
open Minsk.Code_analysis

let red () = prerr_string "\x1b[0;31m"
let reset () = prerr_string "\x1b[0m"

let () =
  let show_tree = ref false in
  let vars = Hashtbl.create 0 in
  let text_builder = ref BatVect.empty in
  try
    while true do
      prerr_string (if BatVect.is_empty !text_builder then "> " else "| ");
      flush stderr;
      match read_line () with
      | "#showTree" when BatVect.is_empty !text_builder ->
          show_tree := not !show_tree;
          (if !show_tree then "Showing parse trees."
           else "Not showing parse trees.")
          |> print_endline
      | "#cls" when BatVect.is_empty !text_builder ->
          print_string "\x1b[2J\x1b[H";
          flush stdout
      | input ->
          let is_blank = input = "" in
          text_builder := BatVect.append input !text_builder;
          let text =
            String.concat "\n" (BatVect.to_list !text_builder) ^ "\n"
          in
          let syntax_tree = Syntax.Tree.parse text in
          if is_blank || Array.length syntax_tree.diagnostics = 0 then (
            (let result = Compilation.evaluate vars syntax_tree in
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
                 Array.iter
                   (fun (d : Diagnostic.t) ->
                     let line_index =
                       Text.Source.line_index syntax_tree.text d.span.start
                     in
                     let line = Text.Source.line syntax_tree.text line_index in
                     let line_number = line_index + 1 in
                     let character = d.span.start - line.start + 1 in
                     red ();
                     Printf.eprintf "(%d, %d): " line_number character;
                     prerr_endline d.message;
                     reset ();
                     let prefix_span =
                       Text.Span.from_bounds line.start d.span.start
                     in
                     let suffix_span =
                       Text.Span.from_bounds (Text.Span.fin d.span)
                         (Text.Line.fin line)
                     in
                     let prefix =
                       Text.Source.sub_span syntax_tree.text prefix_span
                     in
                     let error = Text.Source.sub_span syntax_tree.text d.span in
                     let suffix =
                       Text.Source.sub_span syntax_tree.text suffix_span
                     in
                     prerr_string @@ "    " ^ prefix;
                     red ();
                     prerr_string error;
                     reset ();
                     prerr_endline suffix)
                   diagnostics;
                 flush stderr);
            text_builder := BatVect.empty)
    done
  with End_of_file -> prerr_endline ""
