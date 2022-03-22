package dev.phytolizer.minsk.analysis.syntax

import io.kotest.core.spec.style.FunSpec
import io.kotest.datatest.withData
import io.kotest.matchers.shouldBe

class SyntaxFactsTests : FunSpec({
    context("getText() round trips") {
        withData(SyntaxKind.values().filter { SyntaxFacts.getText(it) != null }) {
            val text = SyntaxFacts.getText(it)!!
            val tokens = SyntaxTree.parseTokens(text)
            tokens.size shouldBe 1
            tokens[0].kind shouldBe it
            tokens[0].text shouldBe text
        }
    }
})
