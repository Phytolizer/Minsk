package dev.phytolizer.minsk.analysis.binding

internal class BoundIfStatement(
    val condition: BoundExpression,
    val thenStatement: BoundStatement,
    val elseStatement: BoundStatement?,
) : BoundStatement() {
    override val kind: BoundNodeKind
        get() = BoundNodeKind.IfStatement
}
