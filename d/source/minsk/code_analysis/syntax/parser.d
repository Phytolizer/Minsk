module minsk.code_analysis.syntax.parser;

import std.algorithm : among, filter;
import std.array : array;
import std.format : format;
import std.range : InputRange;

import minsk.code_analysis.syntax.lexer : Lexer;
import minsk.code_analysis.syntax.syntax_kind : SyntaxKind;
import minsk.code_analysis.syntax.syntax_token : SyntaxToken;
import minsk.code_analysis.syntax.syntax_tree : SyntaxTree;
import minsk.code_analysis.syntax.syntax_node : ExpressionSyntax,
    BinaryExpressionSyntax,
    LiteralExpressionSyntax,
    ParenthesizedExpressionSyntax;

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
        return parseTerm();
    }

    private ExpressionSyntax parseTerm() {
        auto left = parseFactor();

        while (current.kind.among(SyntaxKind.PlusToken, SyntaxKind.MinusToken)) {
            const operatorToken = nextToken();
            const right = parseFactor();
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax parseFactor() {
        auto left = parsePrimaryExpression();

        while (current.kind.among(SyntaxKind.StarToken, SyntaxKind.SlashToken)) {
            const operatorToken = nextToken();
            const right = parsePrimaryExpression();
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
        }

        const numberToken = match(SyntaxKind.NumberToken);
        return new LiteralExpressionSyntax(numberToken);
    }

    const(string[]) diagnostics() const {
        return _diagnostics;
    }
}
