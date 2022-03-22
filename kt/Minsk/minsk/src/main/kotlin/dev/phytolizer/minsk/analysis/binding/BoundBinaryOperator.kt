package dev.phytolizer.minsk.analysis.binding

import dev.phytolizer.minsk.analysis.syntax.SyntaxKind
import java.lang.reflect.Type

internal class BoundBinaryOperator private constructor(
    private val syntaxKind: SyntaxKind,
    val kind: BoundBinaryOperatorKind,
    private val leftType: Type,
    private val rightType: Type,
    val resultType: Type,
) {
    companion object {
        private val operators = listOf(
            BoundBinaryOperator(
                SyntaxKind.PlusToken,
                BoundBinaryOperatorKind.Addition,
                Integer::class.java,
                Integer::class.java,
                Integer::class.java,
            ),
            BoundBinaryOperator(
                SyntaxKind.MinusToken,
                BoundBinaryOperatorKind.Subtraction,
                Integer::class.java,
                Integer::class.java,
                Integer::class.java,
            ),
            BoundBinaryOperator(
                SyntaxKind.StarToken,
                BoundBinaryOperatorKind.Multiplication,
                Integer::class.java,
                Integer::class.java,
                Integer::class.java,
            ),
            BoundBinaryOperator(
                SyntaxKind.SlashToken,
                BoundBinaryOperatorKind.Division,
                Integer::class.java,
                Integer::class.java,
                Integer::class.java,
            )
        )

        fun bind(syntaxKind: SyntaxKind, leftType: Type, rightType: Type): BoundBinaryOperator? {
            return operators.firstOrNull { it.syntaxKind == syntaxKind && it.leftType == leftType && it.rightType == rightType }
        }
    }
}
