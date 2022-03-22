package dev.phytolizer.minsk.analysis.syntax

class Parser(text: String) {
    private val _tokens: List<SyntaxToken>
    private var _position = 0
    private val _diagnostics = mutableListOf<String>()
    val diagnostics: List<String>
        get() = _diagnostics

    init {
        val tempTokens = mutableListOf<SyntaxToken>()
        val lexer = Lexer(text)
        for (token in lexer) {
            if (token.kind != SyntaxKind.BadToken && token.kind != SyntaxKind.WhitespaceToken) {
                tempTokens.add(token)
            }
        }
        tempTokens.add(SyntaxToken(SyntaxKind.EndOfFileToken, tempTokens.lastOrNull()?.position ?: 0, "", null))
        _diagnostics.addAll(lexer.diagnostics)
        _tokens = tempTokens
    }

    private fun peek(offset: Int): SyntaxToken {
        val index = _position + offset
        if (index >= _tokens.size) {
            return _tokens.last()
        }

        return _tokens[index]
    }

    private val current
        get() = peek(0)

    private fun nextToken(): SyntaxToken {
        val curr = current
        _position += 1
        return curr
    }

    private fun matchToken(kind: SyntaxKind): SyntaxToken {
        if (current.kind == kind) {
            return nextToken()
        }

        _diagnostics.add("Expected next token to be $kind, got ${current.kind} instead")
        return SyntaxToken(kind, current.position, "", null)
    }

    fun parse(): SyntaxTree {
        return SyntaxTree(parseExpression(), matchToken(SyntaxKind.EndOfFileToken), diagnostics)
    }

    private fun parseExpression(): ExpressionSyntax {
        return parseBinaryExpression(0)
    }

    private fun parseBinaryExpression(parentPrecedence: Int): ExpressionSyntax {
        val unaryOperatorPrecedence = SyntaxFacts.unaryOperatorPrecedence(current.kind)
        var left = if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence) {
            val operatorToken = nextToken()
            val operand = parseBinaryExpression(unaryOperatorPrecedence)
            UnaryExpressionSyntax(operatorToken, operand)
        } else {
            parsePrimaryExpression()
        }

        while (true) {
            val precedence = SyntaxFacts.binaryOperatorPrecedence(current.kind)
            if (precedence == 0 || precedence <= parentPrecedence) {
                break
            }

            val operatorToken = nextToken()
            val right = parseBinaryExpression(precedence)
            left = BinaryExpressionSyntax(left, operatorToken, right)
        }

        return left
    }

    private fun parsePrimaryExpression(): ExpressionSyntax {
        return if (current.kind == SyntaxKind.OpenParenthesisToken) {
            val openParenthesisToken = matchToken(SyntaxKind.OpenParenthesisToken)
            val expression = parseExpression()
            val closeParenthesisToken = matchToken(SyntaxKind.CloseParenthesisToken)
            ParenthesizedExpressionSyntax(openParenthesisToken, expression, closeParenthesisToken)
        } else if (current.kind == SyntaxKind.TrueKeyword || current.kind == SyntaxKind.FalseKeyword) {
            val isTrue = current.kind == SyntaxKind.TrueKeyword
            val keywordToken = if (isTrue) {
                matchToken(SyntaxKind.TrueKeyword)
            } else {
                matchToken(SyntaxKind.FalseKeyword)
            }
            LiteralExpressionSyntax(keywordToken, isTrue)
        } else {
            val numberToken = matchToken(SyntaxKind.NumberToken)
            LiteralExpressionSyntax(numberToken)
        }
    }
}
