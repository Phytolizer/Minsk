package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.syntax.SyntaxTree
import io.kotest.core.spec.style.FunSpec
import io.kotest.data.forAll
import io.kotest.data.row
import io.kotest.matchers.collections.shouldBeEmpty
import io.kotest.matchers.shouldBe

class EvaluatorTests : FunSpec({
    test("evaluates correct value") {
        forAll<String, Any>(
            row("1", 1),
            row("+1", 1),
            row("-1", -1),
            row("14 + 12", 26),
            row("12 - 3", 9),
            row("4 * 2", 8),
            row("9 / 3", 3),
            row("(10)", 10),
            row("12 == 3", false),
            row("3 == 3", true),
            row("12 != 3", true),
            row("3 != 3", false),
            row("false == false", true),
            row("true == false", false),
            row("false != false", false),
            row("true != false", true),
            row("true", true),
            row("false", false),
            row("!true", false),
            row("!false", true),
            row("(a = 10) * a", 100),
        ) { text, expected ->
            val syntaxTree = SyntaxTree.parse(text)
            val compilation = Compilation(syntaxTree)
            val variables = mutableMapOf<VariableSymbol, Any>()
            val result = compilation.evaluate(variables)

            result.diagnostics.shouldBeEmpty()
            result.value shouldBe expected
        }
    }
})
