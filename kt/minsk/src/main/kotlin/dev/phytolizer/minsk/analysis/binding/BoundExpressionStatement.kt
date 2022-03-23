package dev.phytolizer.minsk.analysis.binding

internal class BoundExpressionStatement(val expression: BoundExpression) : BoundStatement() {
    override val kind: BoundNodeKind
        get() = BoundNodeKind.ExpressionStatement
}
