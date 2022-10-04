package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.syntax.SyntaxTree
import dev.phytolizer.minsk.analysis.text.AnnotatedText
import io.kotest.core.spec.style.FunSpec
import io.kotest.data.forAll
import io.kotest.data.row
import io.kotest.matchers.collections.shouldBeEmpty
import io.kotest.matchers.shouldBe

@Suppress("unused")
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
            row("3 < 4", true),
            row("5 < 4", false),
            row("4 <= 4", true),
            row("4 <= 5", true),
            row("5 <= 4", false),
            row("3 > 4", false),
            row("5 > 4", true),
            row("4 >= 4", true),
            row("4 >= 5", false),
            row("5 >= 4", true),
            row("false == false", true),
            row("true == false", false),
            row("false != false", false),
            row("true != false", true),
            row("true", true),
            row("false", false),
            row("!true", false),
            row("!false", true),
            row("{ var a = 0 (a = 10) * a }", 100),
            row("{ var a = 0 if a == 0 a = 10 a }", 10),
            row("{ var a = 0 if a == 5 a = 10 a }", 0),
            row("{ var a = 0 if a == 0 a = 10 else a = 20 a }", 10),
            row("{ var a = 0 if a == 5 a = 10 else a = 20 a }", 20),
        ) { text, expected ->
            assertValue(text, expected)
        }
    }

    test("variable declaration reports redeclaration") {
        val text = """
            {
                var x = 10
                var y = 100
                {
                    var x = 10
                }
                var [x] = 5
            }
        """
        val diagnostics = """
            Name 'x' is already declared in the same scope
        """
        assertDiagnostics(text, diagnostics)
    }

    test("name expression reports undefined") {
        val text = "[x] + 5"
        val diagnostics = """
            Undefined name 'x'
        """
        assertDiagnostics(text, diagnostics)
    }

    test("assignment expression reports undefined") {
        val text = "[x] = 10"
        val diagnostics = """
            Undefined name 'x'
        """
        assertDiagnostics(text, diagnostics)
    }

    test("assignment expression reports cannot assign") {
        val text = """
            {
                let x = 10
                x [=] 20
            }
        """
        val diagnostics = """
            Cannot assign to read-only variable 'x'
        """
        assertDiagnostics(text, diagnostics)
    }

    test("assignment expression reports cannot convert") {
        val text = """
            {
                var x = 10
                x [=] true
            }
        """
        val diagnostics = """
            Cannot convert from 'class kotlin.Boolean' to 'class kotlin.Int'
        """
        assertDiagnostics(text, diagnostics)
    }

    test("unary operator reports undefined") {
        val text = "[+]true"
        val diagnostics = """
            The unary operator '+' isn't defined for 'class kotlin.Boolean'
        """
        assertDiagnostics(text, diagnostics)
    }

    test("binary operator reports undefined") {
        val text = "true [-] false"
        val diagnostics = """
            The binary operator '-' isn't defined for 'class kotlin.Boolean' and 'class kotlin.Boolean'
        """
        assertDiagnostics(text, diagnostics)
    }
})

private fun assertValue(text: String, expected: Any) {
    val syntaxTree = SyntaxTree.parse(text)
    val compilation = Compilation(syntaxTree)
    val variables = mutableMapOf<VariableSymbol, Any>()
    val result = compilation.evaluate(variables)

    result.diagnostics.shouldBeEmpty()
    result.value shouldBe expected
}

private fun assertDiagnostics(text: String, diagnosticText: String) {
    val annotatedText = AnnotatedText.parse(text)
    val syntaxTree = SyntaxTree.parse(annotatedText.text)
    val compilation = Compilation(syntaxTree)
    val variables = mutableMapOf<VariableSymbol, Any>()
    val result = compilation.evaluate(variables)
    val diagnostics = AnnotatedText.unindentLines(diagnosticText)
    annotatedText.spans.size shouldBe diagnostics.size
    result.diagnostics.size shouldBe diagnostics.size
    for (i in diagnostics.indices) {
        result.diagnostics[i].message shouldBe diagnostics[i]
        result.diagnostics[i].span shouldBe annotatedText.spans[i]
    }
}
