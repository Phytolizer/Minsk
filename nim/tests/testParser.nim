import std/options
import std/strformat
import std/unittest

import minskpkg/codeAnalysis/syntax/[
  parser,
  syntaxFacts,
  syntaxKind,
]

import assertingEnumerator

iterator binaryOperatorPairs(): tuple[op1: SyntaxKind, op2: SyntaxKind] =
  for op1 in binaryOperators():
    for op2 in binaryOperators():
      yield (op1, op2)

suite "parser":
  proc parserBinaryExpressionHonorsPrecedence(op1: SyntaxKind, op2: SyntaxKind) =
    test fmt"binary expression honors precedence ({op1}, {op2})":
      let op1Precedence = op1.binaryOperatorPrecedence
      let op2Precedence = op2.binaryOperatorPrecedence
      let op1Text = op1.text.get
      let op2Text = op2.text.get
      let text = fmt"a {op1Text} b {op2Text} c"
      let expression = parse(text).root

      if op1Precedence >= op2Precedence:
        var e = newAssertingEnumerator(expression)
        e.assertNode(SyntaxKind.BinaryExpression)
        e.assertNode(SyntaxKind.BinaryExpression)
        e.assertNode(SyntaxKind.NameExpression)
        e.assertToken(SyntaxKind.IdentifierToken, "a")
        e.assertToken(op1, op1Text)
        e.assertNode(SyntaxKind.NameExpression)
        e.assertToken(SyntaxKind.IdentifierToken, "b")
        e.assertToken(op2, op2Text)
        e.assertNode(SyntaxKind.NameExpression)
        e.assertToken(SyntaxKind.IdentifierToken, "c")
      else:
        var e = newAssertingEnumerator(expression)
        e.assertNode(SyntaxKind.BinaryExpression)
        e.assertNode(SyntaxKind.NameExpression)
        e.assertToken(SyntaxKind.IdentifierToken, "a")
        e.assertToken(op1, op1Text)
        e.assertNode(SyntaxKind.BinaryExpression)
        e.assertNode(SyntaxKind.NameExpression)
        e.assertToken(SyntaxKind.IdentifierToken, "b")
        e.assertToken(op2, op2Text)
        e.assertNode(SyntaxKind.NameExpression)
        e.assertToken(SyntaxKind.IdentifierToken, "c")

  for (op1, op2) in binaryOperatorPairs():
    parserBinaryExpressionHonorsPrecedence(op1, op2)
