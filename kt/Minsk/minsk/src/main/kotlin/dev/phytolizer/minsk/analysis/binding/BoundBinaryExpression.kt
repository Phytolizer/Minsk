package dev.phytolizer.minsk.analysis.binding

import kotlin.reflect.KClass

internal class BoundBinaryExpression(
    val left: BoundExpression,
    val op: BoundBinaryOperator,
    val right: BoundExpression,
) : BoundExpression() {
    override val type: KClass<out Any>
        get() = op.resultType
    override val kind: BoundNodeKind
        get() = BoundNodeKind.BinaryExpression
}
