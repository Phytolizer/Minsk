module minsk.code_analysis.syntax.parser;

import std.algorithm : among, filter;
import std.array : array;
import std.range : InputRange;

import minsk.code_analysis.syntax.lexer : Lexer;
import minsk.code_analysis.syntax.syntax_kind : SyntaxKind;
import minsk.code_analysis.syntax.syntax_token : SyntaxToken;
import minsk.code_analysis.syntax.syntax_node : ExpressionSyntax,
    BinaryExpressionSyntax,
    LiteralExpressionSyntax;

final class Parser {
    private SyntaxToken[] _tokens;
    private int _position = 0;

    this(string text) {
        _tokens = new Lexer(text)
            .filter!(tok => !tok.kind.among(
                    SyntaxKind.BadToken,
                    SyntaxKind.WhitespaceToken,
            ))
            .array;
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

        return new SyntaxToken(kind, current.position, "", null);
    }

    ExpressionSyntax parse() {
        auto left = parsePrimaryExpression();

        while (current.kind.among(SyntaxKind.PlusToken, SyntaxKind.MinusToken)) {
            const operatorToken = nextToken();
            const right = parsePrimaryExpression();
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax parsePrimaryExpression() {
        const numberToken = match(SyntaxKind.NumberToken);
        return new LiteralExpressionSyntax(numberToken);
    }
}
