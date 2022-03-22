package dev.phytolizer.minsk

class Parser(text: String) {
    private val tokens: List<SyntaxToken>
    private var position = 0

    init {
        val tempTokens = mutableListOf<SyntaxToken>()
        for (token in Lexer(text)) {
            if (token.kind != SyntaxKind.BadToken && token.kind != SyntaxKind.WhitespaceToken) {
                tempTokens.add(token)
            }
        }
        tempTokens.add(SyntaxToken(SyntaxKind.EndOfFileToken, tempTokens.lastOrNull()?.position ?: 0, "", null))
        tokens = tempTokens
    }

    private fun peek(offset: Int): SyntaxToken {
        val index = position + offset
        if (index >= tokens.size) {
            return tokens.last()
        }

        return tokens[index]
    }

    private val current
        get() = peek(0)

    private fun nextToken(): SyntaxToken {
        val curr = current
        position += 1
        return curr
    }

    private fun matchToken(kind: SyntaxKind): SyntaxToken {
        if (current.kind == kind) {
            return nextToken()
        }

        return SyntaxToken(kind, current.position, "", null)
    }

    fun parse(): ExpressionSyntax {
        return parseExpression()
    }

    private fun parseExpression(): ExpressionSyntax {
        return parseTerm()
    }

    private fun parseTerm(): ExpressionSyntax {
        var left = parseFactor()

        while (current.kind == SyntaxKind.PlusToken || current.kind == SyntaxKind.MinusToken) {
            val operatorToken = nextToken()
            val right = parseFactor()
            left = BinaryExpressionSyntax(left, operatorToken, right)
        }

        return left
    }

    private fun parseFactor(): ExpressionSyntax {
        var left = parsePrimaryExpression()

        while (current.kind == SyntaxKind.StarToken || current.kind == SyntaxKind.SlashToken) {
            val operatorToken = nextToken()
            val right = parsePrimaryExpression()
            left = BinaryExpressionSyntax(left, operatorToken, right)
        }

        return left
    }

    private fun parsePrimaryExpression(): ExpressionSyntax {
        val numberToken = matchToken(SyntaxKind.NumberToken)
        return LiteralExpressionSyntax(numberToken)
    }
}
