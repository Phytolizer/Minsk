module minsk.code_analysis.syntax.parser;

import std.algorithm : among, filter;
import std.array : array;
import std.format : format;
import std.range : InputRange;

import minsk.code_analysis.syntax.lexer : Lexer;
import minsk.code_analysis.syntax.facts : binaryPrecedence, unaryPrecedence;
import minsk.code_analysis.syntax.kind : SyntaxKind;
import minsk.code_analysis.syntax.token : SyntaxToken;
import minsk.code_analysis.syntax.tree : SyntaxTree;
import minsk.code_analysis.syntax.node : ExpressionSyntax,
    BinaryExpressionSyntax,
    LiteralExpressionSyntax,
    ParenthesizedExpressionSyntax,
    UnaryExpressionSyntax;
import minsk.runtime.object : Boolean;

final class Parser {
    private SyntaxToken[] _tokens;
    private int _position = 0;
    private string[] _diagnostics;

    this(string text) {
        auto lexer = new Lexer(text);
        _tokens = lexer
            .filter!(tok => !tok.kind.among(
                    SyntaxKind.BadToken,
                    SyntaxKind.WhitespaceToken,
            ))
            .array;
        _diagnostics ~= lexer.diagnostics;
    }

    private SyntaxToken peek(int offset) {
        const index = _position + offset;
        if (index >= _tokens.length) {
            return _tokens[$ - 1];
        }
        return _tokens[index];
    }

    private SyntaxToken current() {
        return peek(0);
    }

    private SyntaxToken nextToken() {
        auto current = current();
        _position++;
        return current;
    }

    private SyntaxToken match(SyntaxKind kind) {
        if (current.kind == kind)
            return nextToken();

        _diagnostics ~= format!"ERROR: Unexpected token <%s>, expected <%s>."(
            current.kind,
            kind,
        );
        return new SyntaxToken(kind, current.position, "", null);
    }

    SyntaxTree parse() {
        const expression = parseExpression();
        const endOfFileToken = match(SyntaxKind.EndOfFileToken);
        return SyntaxTree(expression, endOfFileToken, diagnostics);
    }

    private ExpressionSyntax parseExpression() {
        return parseBinaryExpression();
    }

    private ExpressionSyntax parseBinaryExpression(int parentPrecedence = 0) {
        ExpressionSyntax left;
        const unaryOperatorPrecedence = current.kind.unaryPrecedence;
        if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence) {
            const operatorToken = nextToken();
            const operand = parseBinaryExpression(unaryOperatorPrecedence);
            left = new UnaryExpressionSyntax(operatorToken, operand);
        } else {
            left = parsePrimaryExpression();
        }

        while (true) {
            const precedence = current.kind.binaryPrecedence;
            if (precedence == 0 || precedence <= parentPrecedence) {
                break;
            }

            const operatorToken = nextToken();
            const right = parseBinaryExpression(precedence);
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax parsePrimaryExpression() {
        if (current.kind == SyntaxKind.OpenParenthesisToken) {
            const openParenthesisToken = nextToken();
            const expression = parseExpression();
            const closeParenthesisToken = match(SyntaxKind.CloseParenthesisToken);
            return new ParenthesizedExpressionSyntax(
                openParenthesisToken,
                expression,
                closeParenthesisToken,
            );
        } else if (current.kind.among(SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword)) {
            const keywordToken = nextToken();
            const value = keywordToken.kind == SyntaxKind.TrueKeyword;
            return new LiteralExpressionSyntax(keywordToken, new Boolean(value));
        }

        const numberToken = match(SyntaxKind.NumberToken);
        return new LiteralExpressionSyntax(numberToken);
    }

    const(string[]) diagnostics() const {
        return _diagnostics;
    }
}
