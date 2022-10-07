import textSpan

type
  Diagnostic* = object
    span*: TextSpan
    message*: string

proc newDiagnostic*(span: TextSpan, message: string): Diagnostic =
  result.span = span
  result.message = message

proc `$`*(d: Diagnostic): string =
  d.message
