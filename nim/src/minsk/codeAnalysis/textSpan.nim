type
  TextSpan* = object
    start*: int
    length*: int

proc newTextSpan*(start, length: int): TextSpan =
  result.start = start
  result.length = length

proc fromBounds*(start, stop: int): TextSpan =
  newTextSpan(start, stop - start)

proc getEnd*(span: TextSpan): int =
  span.start + span.length
