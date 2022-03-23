package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.VariableSymbol

internal class BoundVariableDeclaration(val variable: VariableSymbol, val initializer: BoundExpression) :
    BoundStatement() {
    override val kind: BoundNodeKind
        get() = BoundNodeKind.VariableDeclaration
}
