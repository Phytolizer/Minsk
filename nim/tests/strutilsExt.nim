proc readableEscape*(text: string): string =
  result = ""
  for c in text:
    result.addEscapedChar(c)
