package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.TestUtils
import io.kotest.core.spec.style.FunSpec
import io.kotest.datatest.withData
import io.kotest.matchers.types.shouldBeTypeOf

class ParserTests : FunSpec({
    context("binary expression honors precedence") {
        withData(nameFn = { "${it[0]} x ${it[1]}" }, binaryOperatorPairs()) {
            val op1Text = SyntaxFacts.getText(it[0])!!
            val op2Text = SyntaxFacts.getText(it[1])!!
            val op1Precedence = SyntaxFacts.binaryOperatorPrecedence(it[0])
            val op2Precedence = SyntaxFacts.binaryOperatorPrecedence(it[1])

            val text = "a $op1Text b $op2Text c"
            val expression = parseExpression(text)

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

    context("unary operator honors precedence") {
        withData(
            nameFn = { "${it[0]} x ${it[1]}" },
            unaryOperatorPairs(),
        ) {
            val unaryKind = it[0]
            val binaryKind = it[1]
            val unaryText = SyntaxFacts.getText(unaryKind)!!
            val binaryText = SyntaxFacts.getText(binaryKind)!!
            val unaryPrecedence = SyntaxFacts.unaryOperatorPrecedence(unaryKind)
            val binaryPrecedence = SyntaxFacts.binaryOperatorPrecedence(binaryKind)
            val text = "$unaryText a $binaryText b"
            val expression = parseExpression(text)

            if (unaryPrecedence >= binaryPrecedence) {
                val e = AssertingEnumerator(expression)

                e.assertNode(SyntaxKind.BinaryExpression)
                e.assertNode(SyntaxKind.UnaryExpression)
                e.assertToken(unaryKind, unaryText)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "a")
                e.assertToken(binaryKind, binaryText)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "b")
                e.assertAtEnd()
            } else {
                val e = AssertingEnumerator(expression)

                e.assertNode(SyntaxKind.UnaryExpression)
                e.assertToken(unaryKind, unaryText)
                e.assertNode(SyntaxKind.BinaryExpression)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "a")
                e.assertToken(binaryKind, binaryText)
                e.assertNode(SyntaxKind.NameExpression)
                e.assertToken(SyntaxKind.IdentifierToken, "b")
                e.assertAtEnd()
            }
        }
    }
}) {
    companion object {
        private fun binaryOperatorPairs() =
            TestUtils.cartesianProduct(SyntaxFacts.binaryOperators(), SyntaxFacts.binaryOperators())

        private fun unaryOperatorPairs() =
            TestUtils.cartesianProduct(SyntaxFacts.unaryOperators(), SyntaxFacts.binaryOperators())

        private fun parseExpression(text: String): ExpressionSyntax =
            SyntaxTree.parse(text).root.statement.shouldBeTypeOf<ExpressionStatementSyntax>().expression
    }
}
