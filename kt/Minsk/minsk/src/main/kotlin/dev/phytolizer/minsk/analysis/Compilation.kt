package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.binding.Binder
import dev.phytolizer.minsk.analysis.syntax.SyntaxTree

class Compilation {
    fun evaluate(syntax: SyntaxTree): EvaluationResult {
        val binder = Binder()
        val expression = binder.bindExpression(syntax.root)
        val diagnostics = listOf(syntax.diagnostics, binder.diagnostics).flatten()
        return if (diagnostics.isEmpty()) {
            EvaluationResult(Evaluator().evaluate(expression), listOf())
        } else {
            EvaluationResult(null, diagnostics)
        }
    }
}
