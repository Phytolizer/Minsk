package dev.phytolizer.minsk.analysis.binding

internal class BoundBlockStatement(val statements: List<BoundStatement>) : BoundStatement() {
    override val kind: BoundNodeKind
        get() = BoundNodeKind.BlockStatement
}

