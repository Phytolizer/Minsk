package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.binding.Binder
import dev.phytolizer.minsk.analysis.syntax.SyntaxTree

class Compilation {
    fun evaluate(syntax: SyntaxTree, variables: MutableMap<VariableSymbol, Any>): EvaluationResult {
        val globalScope = Binder.bindGlobalScope(syntax.root)
        val expression = globalScope.expression
        val diagnostics = listOf(syntax.diagnostics, globalScope.diagnostics).flatten()
        return if (diagnostics.isEmpty()) {
            EvaluationResult(Evaluator(variables).evaluate(expression), listOf())
        } else {
            EvaluationResult(null, diagnostics)
        }
    }
}
