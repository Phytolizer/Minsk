package dev.phytolizer.minsk.analysis

import dev.phytolizer.minsk.analysis.binding.Binder
import dev.phytolizer.minsk.analysis.binding.BoundGlobalScope
import dev.phytolizer.minsk.analysis.syntax.SyntaxTree
import java.util.concurrent.atomic.AtomicReference

class Compilation private constructor(val previous: Compilation?, val syntax: SyntaxTree) {
    private var _globalScope = AtomicReference<BoundGlobalScope?>()
    internal val globalScope: BoundGlobalScope
        get() {
            _globalScope.compareAndSet(null, Binder.bindGlobalScope(previous?.globalScope, syntax.root))

            return _globalScope.get()!!
        }

    constructor(syntax: SyntaxTree) : this(null, syntax)

    fun continueWith(syntax: SyntaxTree): Compilation {
        return Compilation(this, syntax)
    }

    fun evaluate(variables: MutableMap<VariableSymbol, Any>): EvaluationResult {
        val statement = globalScope.statement
        val diagnostics = listOf(syntax.diagnostics, globalScope.diagnostics).flatten()
        return if (diagnostics.isEmpty()) {
            EvaluationResult(Evaluator(variables).evaluate(statement), listOf())
        } else {
            EvaluationResult(null, diagnostics)
        }
    }
}
