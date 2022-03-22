package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.TestUtils
import io.kotest.core.spec.style.FunSpec
import io.kotest.datatest.withData

class ParserTests : FunSpec({
    context("binary expression honors precedence") {
        withData(nameFn = { "${it[0]} x ${it[1]}" }, binaryOperatorPairs()) {
            val op1Text = SyntaxFacts.getText(it[0])!!
            val op2Text = SyntaxFacts.getText(it[1])!!
            val op1Precedence = SyntaxFacts.binaryOperatorPrecedence(it[0])
            val op2Precedence = SyntaxFacts.binaryOperatorPrecedence(it[1])

            val text = "a $op1Text b $op2Text c"
            val expression = SyntaxTree.parse(text).root

            if (op1Precedence >= op2Precedence) {
                val e = AssertingEnumerator(expression)

                e.assertNode(SyntaxKind.BinaryExpression)
                e.assertNode(SyntaxKind.BinaryExpression)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "a")
                e.assertToken(it[0], op1Text)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "b")
                e.assertToken(it[1], op2Text)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "c")
                e.assertAtEnd()
            } else {
                val e = AssertingEnumerator(expression)

                e.assertNode(SyntaxKind.BinaryExpression)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "a")
                e.assertToken(it[0], op1Text)
                e.assertNode(SyntaxKind.BinaryExpression)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "b")
                e.assertToken(it[1], op2Text)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "c")
                e.assertAtEnd()
            }
        }
    }
}) {
    companion object {
        fun binaryOperatorPairs() =
            TestUtils.cartesianProduct(SyntaxFacts.binaryOperators(), SyntaxFacts.binaryOperators())
    }
}
