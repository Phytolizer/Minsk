package dev.phytolizer.minsk.analysis.syntax

import dev.phytolizer.minsk.analysis.DiagnosticBag
import dev.phytolizer.minsk.analysis.text.SourceText

internal class Parser(_text: SourceText) {
    private val _tokens: List<SyntaxToken>
    private var _position = 0
    private val _diagnostics = DiagnosticBag()
    val diagnostics = _diagnostics.toList()

    init {
        val tempTokens = mutableListOf<SyntaxToken>()
        val lexer = Lexer(_text)
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

    fun parseCompilationUnit(): CompilationUnitSyntax {
        val statement = parseStatement()
        val endOfFileToken = matchToken(SyntaxKind.EndOfFileToken)
        return CompilationUnitSyntax(statement, endOfFileToken)
    }

    private fun parseStatement(): StatementSyntax = when (current.kind) {
        SyntaxKind.OpenBraceToken -> parseBlockStatement()
        SyntaxKind.LetKeyword, SyntaxKind.VarKeyword -> parseVariableDeclaration()
        SyntaxKind.IfKeyword -> parseIfStatement()
        else -> parseExpressionStatement()
    }

    private fun parseBlockStatement(): StatementSyntax {
        val openBraceToken = matchToken(SyntaxKind.OpenBraceToken)
        val statements = mutableListOf<StatementSyntax>()
        while (current.kind != SyntaxKind.CloseBraceToken && current.kind != SyntaxKind.EndOfFileToken) {
            statements.add(parseStatement())
        }
        val closeBraceToken = matchToken(SyntaxKind.CloseBraceToken)
        return BlockStatementSyntax(openBraceToken, statements, closeBraceToken)
    }

    private fun parseIfStatement(): StatementSyntax {
        val ifKeyword = matchToken(SyntaxKind.IfKeyword)
        val condition = parseExpression()
        val thenStatement = parseStatement()
        val elseClause = parseElseClause()
        return IfStatementSyntax(ifKeyword, condition, thenStatement, elseClause)
    }

    private fun parseElseClause(): ElseClauseSyntax? {
        if (current.kind != SyntaxKind.ElseKeyword) {
            return null
        }

        val elseKeyword = nextToken()
        val elseStatement = parseStatement()
        return ElseClauseSyntax(elseKeyword, elseStatement)
    }

    private fun parseVariableDeclaration(): StatementSyntax {
        val expected = if (current.kind == SyntaxKind.LetKeyword) {
            SyntaxKind.LetKeyword
        } else {
            SyntaxKind.VarKeyword
        }
        val keywordToken = matchToken(expected)
        val identifierToken = matchToken(SyntaxKind.IdentifierToken)
        val equalsToken = matchToken(SyntaxKind.EqualsToken)
        val initializer = parseExpression()
        return VariableDeclarationSyntax(keywordToken, identifierToken, equalsToken, initializer)
    }

    private fun parseExpressionStatement(): StatementSyntax {
        val expression = parseExpression()
        return ExpressionStatementSyntax(expression)
    }

    private fun parseExpression(): ExpressionSyntax {
        return parseAssignmentExpression()
    }

    private fun parseAssignmentExpression(): ExpressionSyntax {
        return if (peek(0).kind == SyntaxKind.IdentifierToken && peek(1).kind == SyntaxKind.EqualsToken) {
            val identifierToken = nextToken()
            val equalsToken = nextToken()
            val expression = parseAssignmentExpression()
            AssignmentExpressionSyntax(identifierToken, equalsToken, expression)
        } else {
            parseBinaryExpression(0)
        }
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
        SyntaxKind.NumberToken -> parseNumberLiteral()
        else -> parseNameExpression()
    }

    private fun parseNameExpression(): ExpressionSyntax {
        val identifierToken = matchToken(SyntaxKind.IdentifierToken)
        return NameExpressionSyntax(identifierToken)
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
