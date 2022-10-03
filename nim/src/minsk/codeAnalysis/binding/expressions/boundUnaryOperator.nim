import std/options

import minsk/minskObject
import minsk/codeAnalysis/syntax/syntaxKind
import sequtilsExt/sequtilsExt

type
  BoundUnaryOperatorKind* {.pure.} = enum
    Identity
    Negation
    LogicalNegation

  BoundUnaryOperator* = object
    syntaxKind: SyntaxKind
    kind*: BoundUnaryOperatorKind
    operandType: MinskObjectKind
    resultType*: MinskObjectKind

const
  BOUND_UNARY_OPERATORS = [
    BoundUnaryOperator(
      syntaxKind: SyntaxKind.PlusToken,
      kind: BoundUnaryOperatorKind.Identity,
      operandType: mokInteger,
      resultType: mokInteger,
    ),
    BoundUnaryOperator(
      syntaxKind: SyntaxKind.MinusToken,
      kind: BoundUnaryOperatorKind.Negation,
      operandType: mokInteger,
      resultType: mokInteger,
    ),
    BoundUnaryOperator(
      syntaxKind: SyntaxKind.BangToken,
      kind: BoundUnaryOperatorKind.LogicalNegation,
      operandType: mokBoolean,
      resultType: mokBoolean,
    ),
  ]

proc bindUnaryOperator*(
  kind: SyntaxKind,
  operandType: MinskObjectKind
): Option[BoundUnaryOperator] =
  sequtilsExt.find(
    BOUND_UNARY_OPERATORS,
    proc(x: BoundUnaryOperator): bool =
      x.syntaxKind == kind and x.operandType == operandType
  )
