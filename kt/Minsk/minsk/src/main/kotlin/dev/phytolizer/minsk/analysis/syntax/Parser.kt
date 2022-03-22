package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.DiagnosticBag

internal class Parser(text: String) {
    private val _tokens: List<SyntaxToken>
    private var _position = 0
    private val _diagnostics = DiagnosticBag()

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

        _diagnostics.reportUnexpectedToken(current.span, kind, current.kind)
        return SyntaxToken(kind, current.position, "", null)
    }

    fun parse(): SyntaxTree {
        return SyntaxTree(parseExpression(), matchToken(SyntaxKind.EndOfFileToken), _diagnostics.diagnostics)
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

    private fun parsePrimaryExpression(): ExpressionSyntax = when (current.kind) {
        SyntaxKind.OpenParenthesisToken -> parseParenthesizedExpression()
        SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword -> parseBooleanLiteral()
        else -> parseNumberLiteral()
    }

    private fun parseNumberLiteral(): LiteralExpressionSyntax {
        val numberToken = matchToken(SyntaxKind.NumberToken)
        return LiteralExpressionSyntax(numberToken)
    }

    private fun parseBooleanLiteral(): LiteralExpressionSyntax {
        val isTrue = current.kind == SyntaxKind.TrueKeyword
        val keywordToken = if (isTrue) {
            matchToken(SyntaxKind.TrueKeyword)
        } else {
            matchToken(SyntaxKind.FalseKeyword)
        }
        return LiteralExpressionSyntax(keywordToken, isTrue)
    }

    private fun parseParenthesizedExpression(): ParenthesizedExpressionSyntax {
        val openParenthesisToken = matchToken(SyntaxKind.OpenParenthesisToken)
        val expression = parseExpression()
        val closeParenthesisToken = matchToken(SyntaxKind.CloseParenthesisToken)
        return ParenthesizedExpressionSyntax(openParenthesisToken, expression, closeParenthesisToken)
    }
}
