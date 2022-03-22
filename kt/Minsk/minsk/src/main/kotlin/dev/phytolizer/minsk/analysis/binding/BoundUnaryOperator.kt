package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.syntax.SyntaxKind
import java.lang.reflect.Type

internal class BoundUnaryOperator private constructor(
    private val syntaxKind: SyntaxKind,
    val kind: BoundUnaryOperatorKind,
    private val operandType: Type,
    val resultType: Type,
) {
    companion object {
        private val operators = listOf(
            BoundUnaryOperator(
                SyntaxKind.PlusToken,
                BoundUnaryOperatorKind.Identity,
                Int::class.java,
                Int::class.java
            ),
            BoundUnaryOperator(
                SyntaxKind.MinusToken,
                BoundUnaryOperatorKind.Negation,
                Int::class.java,
                Int::class.java
            ),
        )

        fun bind(syntaxKind: SyntaxKind, operandType: Type): BoundUnaryOperator? {
            return operators.firstOrNull { it.syntaxKind == syntaxKind && it.operandType == operandType }
        }
    }
}
