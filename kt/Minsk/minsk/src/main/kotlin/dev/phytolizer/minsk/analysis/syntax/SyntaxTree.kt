package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.Evaluator
import dev.phytolizer.minsk.analysis.binding.Binder

class SyntaxTree(val root: ExpressionSyntax, val endOfFileToken: SyntaxToken, val diagnostics: List<String>) {
    companion object {
        fun parse(text: String): SyntaxTree {
            return Parser(text).parse()
        }
    }

    fun evaluate(): EvaluationResult {
        val binder = Binder()
        val expression = binder.bindExpression(root)
        return if (binder.diagnostics.isEmpty()) {
            EvaluationResult(Evaluator().evaluate(expression), listOf())
        } else {
            EvaluationResult(null, binder.diagnostics)
        }
    }
}
