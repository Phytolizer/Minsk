package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.Diagnostic
import dev.phytolizer.minsk.analysis.VariableSymbol

internal class BoundGlobalScope(
    val previous: BoundGlobalScope?,
    val diagnostics: List<Diagnostic>,
    val variables: List<VariableSymbol>,
    val expression: BoundExpression,
)
