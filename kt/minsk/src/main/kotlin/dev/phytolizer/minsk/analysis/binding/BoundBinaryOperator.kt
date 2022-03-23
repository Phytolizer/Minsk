package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.syntax.SyntaxKind
import kotlin.reflect.KClass

internal class BoundBinaryOperator private constructor(
    private val syntaxKind: SyntaxKind,
    val kind: BoundBinaryOperatorKind,
    private val leftType: KClass<out Any>,
    private val rightType: KClass<out Any>,
    val resultType: KClass<out Any>,
) {
    companion object {
        private val operators = listOf(
            BoundBinaryOperator(
                SyntaxKind.PlusToken,
                BoundBinaryOperatorKind.Addition,
                Int::class,
                Int::class,
                Int::class,
            ),
            BoundBinaryOperator(
                SyntaxKind.MinusToken,
                BoundBinaryOperatorKind.Subtraction,
                Int::class,
                Int::class,
                Int::class,
            ),
            BoundBinaryOperator(
                SyntaxKind.StarToken,
                BoundBinaryOperatorKind.Multiplication,
                Int::class,
                Int::class,
                Int::class,
            ),
            BoundBinaryOperator(
                SyntaxKind.SlashToken,
                BoundBinaryOperatorKind.Division,
                Int::class,
                Int::class,
                Int::class,
            ),
            BoundBinaryOperator(
                SyntaxKind.AmpersandAmpersandToken,
                BoundBinaryOperatorKind.LogicalAnd,
                Boolean::class,
                Boolean::class,
                Boolean::class,
            ),
            BoundBinaryOperator(
                SyntaxKind.PipePipeToken,
                BoundBinaryOperatorKind.LogicalOr,
                Boolean::class,
                Boolean::class,
                Boolean::class,
            ),
            BoundBinaryOperator(
                SyntaxKind.EqualsEqualsToken,
                BoundBinaryOperatorKind.Equality,
                Int::class,
                Int::class,
                Boolean::class,
            ),
            BoundBinaryOperator(
                SyntaxKind.BangEqualsToken,
                BoundBinaryOperatorKind.Inequality,
                Int::class,
                Int::class,
                Boolean::class,
            ),
            BoundBinaryOperator(
                SyntaxKind.EqualsEqualsToken,
                BoundBinaryOperatorKind.Equality,
                Boolean::class,
                Boolean::class,
                Boolean::class,
            ),
            BoundBinaryOperator(
                SyntaxKind.BangEqualsToken,
                BoundBinaryOperatorKind.Inequality,
                Boolean::class,
                Boolean::class,
                Boolean::class,
            ),
        )

        fun bind(syntaxKind: SyntaxKind, leftType: KClass<out Any>, rightType: KClass<out Any>): BoundBinaryOperator? {
            return operators.firstOrNull { it.syntaxKind == syntaxKind && it.leftType == leftType && it.rightType == rightType }
        }
    }
}
