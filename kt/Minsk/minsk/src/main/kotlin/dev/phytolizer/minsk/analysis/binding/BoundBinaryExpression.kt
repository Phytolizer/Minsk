package dev.phytolizer.minsk.analysis.binding

import java.lang.reflect.Type

internal class BoundBinaryExpression(
    val left: BoundExpression,
    val op: BoundBinaryOperator,
    val right: BoundExpression,
) : BoundExpression() {
    override val type: Type
        get() = op.resultType
    override val kind: BoundNodeKind
        get() = BoundNodeKind.BinaryExpression
}
