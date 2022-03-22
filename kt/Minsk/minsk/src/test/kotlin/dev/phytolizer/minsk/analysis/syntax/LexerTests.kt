package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.TestUtils
import io.kotest.core.spec.style.FunSpec
import io.kotest.datatest.withData
import io.kotest.matchers.shouldBe

class LexerTests : FunSpec({
    context("lexes token") {
        withData(tokens()) { (kind, text) ->
            val tokens = SyntaxTree.parseTokens(text)
            tokens.size shouldBe 1
            tokens[0].kind shouldBe kind
            tokens[0].text shouldBe text
        }
    }

    context("lexes token pairs") {
        withData(
            nameFn = { "${it[0].kind} (${it[0].text}) x ${it[1].kind} (${it[1].text})" },
            tokenPairs(),
        ) { tt ->
            val text = "${tt[0].text}${tt[1].text}"
            val tokens = SyntaxTree.parseTokens(text)
            tokens.size shouldBe 2
            tokens[0].kind shouldBe tt[0].kind
            tokens[0].text shouldBe tt[0].text
            tokens[1].kind shouldBe tt[1].kind
            tokens[1].text shouldBe tt[1].text
        }
    }

    context("lexes token pairs with separator") {
        withData(
            nameFn = { "${it[0].kind} (${it[0].text}) x sep x ${it[2].kind} (${it[2].text})" },
            tokenPairsWithSeparator(),
        ) { tt ->
            val text = "${tt[0].text}${tt[1].text}${tt[2].text}"
            val tokens = SyntaxTree.parseTokens(text)
            tokens.size shouldBe 3
            tokens[0].kind shouldBe tt[0].kind
            tokens[0].text shouldBe tt[0].text
            tokens[1].kind shouldBe tt[1].kind
            tokens[1].text shouldBe tt[1].text
            tokens[2].kind shouldBe tt[2].kind
            tokens[2].text shouldBe tt[2].text
        }
    }
}) {
    companion object {
        private fun tokens(): List<SimpleToken> {
            val fixedTokens = SyntaxKind.values().filter { SyntaxFacts.getText(it) != null }
                .map { SimpleToken(it, SyntaxFacts.getText(it)!!) }
            return listOf(
                fixedTokens,
                listOf(
                    SimpleToken(SyntaxKind.IdentifierToken, "a"),
                    SimpleToken(SyntaxKind.IdentifierToken, "abc"),
                    SimpleToken(SyntaxKind.NumberToken, "1"),
                    SimpleToken(SyntaxKind.NumberToken, "123"),
                ),
            ).flatten()
        }

        private fun separators(): List<SimpleToken> = listOf(
            SimpleToken(SyntaxKind.WhitespaceToken, " "),
            SimpleToken(SyntaxKind.WhitespaceToken, "  "),
            SimpleToken(SyntaxKind.WhitespaceToken, "\r"),
            SimpleToken(SyntaxKind.WhitespaceToken, "\n"),
            SimpleToken(SyntaxKind.WhitespaceToken, "\r\n"),
        )

        private fun tokenPairs() =
            TestUtils.cartesianProduct(tokens(), tokens()).filter { !requiresSeparator(it[0], it[1]) }

        private fun tokenPairsWithSeparator() = TestUtils.cartesianProduct(
            tokens(),
            separators(),
            tokens(),
        ).filter { requiresSeparator(it[0], it[2]) }

        private fun requiresSeparator(t1: SimpleToken, t2: SimpleToken): Boolean {
            val t1IsKeyword = t1.kind.toString().endsWith("Keyword")
            val t2IsKeyword = t2.kind.toString().endsWith("Keyword")

            return if ((t1.kind == SyntaxKind.IdentifierToken || t1IsKeyword) && (t2.kind == SyntaxKind.IdentifierToken || t2IsKeyword)) {
                true
            } else if ((t1.kind == SyntaxKind.NumberToken || t1.kind == SyntaxKind.IdentifierToken || t1IsKeyword) && t2.kind == SyntaxKind.NumberToken) {
                true
            } else (t1.kind == SyntaxKind.BangToken || t1.kind == SyntaxKind.EqualsToken) && (t2.kind == SyntaxKind.EqualsToken || t2.kind == SyntaxKind.EqualsEqualsToken)
        }
    }

    data class SimpleToken(val kind: SyntaxKind, val text: String)
}

