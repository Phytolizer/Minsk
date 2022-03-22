package dev.phytolizer.minsk.analysis.binding

import kotlin.reflect.KClass

internal abstract class BoundExpression : BoundNode() {
    abstract val type: KClass<out Any>
}

