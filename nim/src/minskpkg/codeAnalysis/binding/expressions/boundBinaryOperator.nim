import std/options

import minskpkg/minskObject
import minskpkg/codeAnalysis/syntax/syntaxKind
import sequtilsExt/sequtilsExt

type
  BoundBinaryOperatorKind* {.pure.} = enum
    Addition
    Subtraction
    Multiplication
    Division
    LogicalAnd
    LogicalOr
    Equality
    Inequality

  BoundBinaryOperator* = object
    syntaxKind: SyntaxKind
    kind*: BoundBinaryOperatorKind
    leftType: MinskObjectKind
    rightType: MinskObjectKind
    resultType*: MinskObjectKind

const
  BOUND_BINARY_OPERATORS = [
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.PlusToken,
      kind: BoundBinaryOperatorKind.Addition,
      leftType: mokInteger,
      rightType: mokInteger,
      resultType: mokInteger,
    ),
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.MinusToken,
      kind: BoundBinaryOperatorKind.Subtraction,
      leftType: mokInteger,
      rightType: mokInteger,
      resultType: mokInteger,
    ),
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.StarToken,
      kind: BoundBinaryOperatorKind.Multiplication,
      leftType: mokInteger,
      rightType: mokInteger,
      resultType: mokInteger,
    ),
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.SlashToken,
      kind: BoundBinaryOperatorKind.Division,
      leftType: mokInteger,
      rightType: mokInteger,
      resultType: mokInteger,
    ),
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.AmpersandAmpersandToken,
      kind: BoundBinaryOperatorKind.LogicalAnd,
      leftType: mokBoolean,
      rightType: mokBoolean,
      resultType: mokBoolean,
    ),
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.PipePipeToken,
      kind: BoundBinaryOperatorKind.LogicalOr,
      leftType: mokBoolean,
      rightType: mokBoolean,
      resultType: mokBoolean,
    ),
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.EqualsEqualsToken,
      kind: BoundBinaryOperatorKind.Equality,
      leftType: mokInteger,
      rightType: mokInteger,
      resultType: mokBoolean,
    ),
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.BangEqualsToken,
      kind: BoundBinaryOperatorKind.Inequality,
      leftType: mokInteger,
      rightType: mokInteger,
      resultType: mokBoolean,
    ),
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.EqualsEqualsToken,
      kind: BoundBinaryOperatorKind.Equality,
      leftType: mokBoolean,
      rightType: mokBoolean,
      resultType: mokBoolean,
    ),
    BoundBinaryOperator(
      syntaxKind: SyntaxKind.BangEqualsToken,
      kind: BoundBinaryOperatorKind.Inequality,
      leftType: mokBoolean,
      rightType: mokBoolean,
      resultType: mokBoolean,
    ),
  ]

proc bindBinaryOperator*(
  kind: SyntaxKind,
  leftType: MinskObjectKind,
  rightType: MinskObjectKind
): Option[BoundBinaryOperator] =
  sequtilsExt.find(
    BOUND_BINARY_OPERATORS,
    proc (op: BoundBinaryOperator): bool =
      op.syntaxKind == kind and op.leftType == leftType and op.rightType == rightType
  )
