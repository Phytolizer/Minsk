package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.syntax.SyntaxKind
import kotlin.reflect.KClass

internal class BoundUnaryOperator private constructor(
    private val syntaxKind: SyntaxKind,
    val kind: BoundUnaryOperatorKind,
    private val operandType: KClass<out Any>,
    val resultType: KClass<out Any>,
) {
    companion object {
        private val operators = listOf(
            BoundUnaryOperator(SyntaxKind.PlusToken, BoundUnaryOperatorKind.Identity, Int::class, Int::class),
            BoundUnaryOperator(SyntaxKind.MinusToken, BoundUnaryOperatorKind.Negation, Int::class, Int::class),
            BoundUnaryOperator(
                SyntaxKind.BangToken,
                BoundUnaryOperatorKind.LogicalNegation,
                Boolean::class,
                Boolean::class,
            ),
        )

        fun bind(syntaxKind: SyntaxKind, operandType: KClass<out Any>): BoundUnaryOperator? {
            return operators.firstOrNull { it.syntaxKind == syntaxKind && it.operandType == operandType }
        }
    }
}
