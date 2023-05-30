type t = { text : string; lines : Line.t array }

let line_break_width text position =
  match text.[position] with
  | '\r' when position + 1 < String.length text && text.[position + 1] = '\n' ->
      2
  | '\r' | '\n' -> 1
  | _ -> 0

let add_line result position line_start break_width =
  let line_length = position - line_start in
  let line_length_including_line_break = line_length + break_width in
  let line =
    Line.make ~start:line_start ~length:line_length
      ~length_including_line_break:line_length_including_line_break
  in
  result := BatVect.append line !result

let parse_lines text =
  let result = ref BatVect.empty in
  let position = ref 0 in
  let line_start = ref 0 in
  while !position < String.length text do
    match line_break_width text !position with
    | 0 -> position := !position + 1
    | width ->
        add_line result !position !line_start width;
        position := !position + width;
        line_start := !position
  done;
  if !position >= !line_start then add_line result !position !line_start 0;
  BatVect.to_array !result

let make text = { text; lines = parse_lines text }
let sub self start length = String.sub self.text start length
let sub_span self (span : Span.t) = sub self span.start span.length
let length self = String.length self.text
let line self i = self.lines.(i)

let line_index self position =
  let rec loop lower upper =
    if lower > upper then lower - 1
    else
      let index = (lower + upper) / 2 in
      let start = self.lines.(index).start in
      if start = position then index
      else if start > position then loop lower (index - 1)
      else loop (index + 1) upper
  in
  loop 0 (Array.length self.lines - 1)

let ref self i = self.text.[i]
