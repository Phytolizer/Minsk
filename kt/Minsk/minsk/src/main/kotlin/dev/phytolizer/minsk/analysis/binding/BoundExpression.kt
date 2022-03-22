package dev.phytolizer.minsk.analysis.binding

import java.lang.reflect.Type

internal abstract class BoundExpression : BoundNode() {
    abstract val type: Type
}

