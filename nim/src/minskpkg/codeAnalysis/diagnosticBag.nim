import std/strformat

import minskpkg/minskObject
import syntax/syntaxKind

import diagnostic
import textSpan

type
  DiagnosticBag* = object
    mDiagnostics: seq[Diagnostic]

proc newDiagnosticBag*(): DiagnosticBag =
  result.mDiagnostics = @[]

iterator diagnostics*(bag: DiagnosticBag): Diagnostic =
  for d in bag.mDiagnostics:
    yield d

proc add*(bag: var DiagnosticBag, other: seq[Diagnostic]) =
  bag.mDiagnostics.add other

proc report(bag: var DiagnosticBag, span: TextSpan, message: string) =
  bag.mDiagnostics.add newDiagnostic(span, message)

proc reportInvalidInteger*(
  bag: var DiagnosticBag,
  span: TextSpan,
  text: string,
  ty: MinskObjectKind
) =
  bag.report(span, fmt"The number {text} isn't a valid {ty}.")

proc reportBadCharacter*(bag: var DiagnosticBag, position: int, c: char) =
  bag.report(newTextSpan(position, 1), fmt"Bad character input: '{c}'.")

proc reportUnexpectedToken*(
  bag: var DiagnosticBag,
  span: TextSpan,
  actualKind: SyntaxKind,
  expectedKind: SyntaxKind
) =
  bag.report(span, fmt"Unexpected token <{actualKind}>, expected <{expectedKind}>.")

proc reportUndefinedBinaryOperator*(
  bag: var DiagnosticBag,
  span: TextSpan,
  operatorText: string,
  leftType: MinskObjectKind,
  rightType: MinskObjectKind
) =
  let message =
    fmt"Binary operator '{operatorText}' is not defined " &
    fmt"for types '{leftType}' and '{rightType}'."
  bag.report(span, message)

proc reportUndefinedUnaryOperator*(
  bag: var DiagnosticBag,
  span: TextSpan,
  operatorText: string,
  operandType: MinskObjectKind
) =
  let message =
    fmt"Unary operator '{operatorText}' is not defined for type '{operandType}'."
  bag.report(span, message)

proc reportUndefinedName*(
  bag: var DiagnosticBag,
  span: TextSpan,
  name: string
) =
  bag.report(span, fmt"Variable '{name}' doesn't exist.")
