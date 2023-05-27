type t = { start : int; length : int; length_including_line_break : int }

let make ~start ~length ~length_including_line_break =
  { start; length; length_including_line_break }

let fin self = self.start + self.length
let span self = Span.make self.start self.length

let span_including_line_break self =
  Span.make self.start self.length_including_line_break
