package dev.phytolizer.minsk.analysis.binding

import java.lang.reflect.Type

internal class BoundLiteralExpression(val value: Any) : BoundExpression() {
    override val type: Type
        get() = value.javaClass
    override val kind: BoundNodeKind
        get() = BoundNodeKind.LiteralExpression
}
