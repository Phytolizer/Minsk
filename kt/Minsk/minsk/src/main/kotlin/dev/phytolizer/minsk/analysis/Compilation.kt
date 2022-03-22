package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.binding.Binder
import dev.phytolizer.minsk.analysis.syntax.SyntaxTree

class Compilation {
    fun evaluate(syntax: SyntaxTree, variables: MutableMap<String, Any>): EvaluationResult {
        val binder = Binder(variables)
        val expression = binder.bindExpression(syntax.root)
        val diagnostics = listOf(syntax.diagnostics, binder.diagnostics).flatten()
        return if (diagnostics.isEmpty()) {
            EvaluationResult(Evaluator(variables).evaluate(expression), listOf())
        } else {
            EvaluationResult(null, diagnostics)
        }
    }
}
