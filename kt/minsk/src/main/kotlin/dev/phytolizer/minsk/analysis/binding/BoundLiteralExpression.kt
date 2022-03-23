package dev.phytolizer.minsk.analysis.binding

import kotlin.reflect.KClass

internal class BoundLiteralExpression(val value: Any) : BoundExpression() {
    override val type: KClass<out Any>
        get() = value::class
    override val kind: BoundNodeKind
        get() = BoundNodeKind.LiteralExpression
}
