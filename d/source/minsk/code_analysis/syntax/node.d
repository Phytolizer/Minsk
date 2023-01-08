module minsk.code_analysis.syntax.node;

import std.range : takeOne, front;
import std.stdio : write, writeln;

import minsk.code_analysis.syntax.kind : SyntaxKind;
import minsk.code_analysis.syntax.token : SyntaxToken;

interface SyntaxNode {
    SyntaxKind kind() const;
    const(SyntaxNode)[] children() const;
}

void prettyPrint(
    const(SyntaxNode) node,
    string indent = "",
    bool isLast = true,
) {
    write(indent);
    write(isLast ? "└── " : "├── ");
    write(node.kind);
    if (auto t = cast(SyntaxToken) node)
        if (t.value) {
            write(" ");
            write(t.value);
        }
    writeln();

    indent ~= isLast ? "    " : "│   ";
    const children = node.children;
    foreach (i, child; children) {
        prettyPrint(child, indent, i + 1 == children.length);
    }
}

interface ExpressionSyntax : SyntaxNode {
}

final class LiteralExpressionSyntax : ExpressionSyntax {
    private const(SyntaxToken) _literalToken;

    this(const(SyntaxToken) literalToken) {
        _literalToken = literalToken;
    }

    const(SyntaxToken) literalToken() const @property {
        return _literalToken;
    }

    override SyntaxKind kind() const {
        return SyntaxKind.LiteralExpression;
    }

    override const(SyntaxNode)[] children() const {
        return [literalToken];
    }
}

final class BinaryExpressionSyntax : ExpressionSyntax {
    private const(ExpressionSyntax) _left;
    private const(SyntaxToken) _operatorToken;
    private const(ExpressionSyntax) _right;

    this(
        const(ExpressionSyntax) left,
        const(SyntaxToken) operatorToken,
        const(ExpressionSyntax) right,
    ) {
        _left = left;
        _operatorToken = operatorToken;
        _right = right;
    }

    const(ExpressionSyntax) left() const @property {
        return _left;
    }

    const(SyntaxToken) operatorToken() const @property {
        return _operatorToken;
    }

    const(ExpressionSyntax) right() const @property {
        return _right;
    }

    override SyntaxKind kind() const {
        return SyntaxKind.BinaryExpression;
    }

    override const(SyntaxNode)[] children() const {
        return [cast(SyntaxNode) left, operatorToken, right];
    }
}

final class ParenthesizedExpressionSyntax : ExpressionSyntax {
    private const(SyntaxToken) _openParenthesisToken;
    private const(ExpressionSyntax) _expression;
    private const(SyntaxToken) _closeParenthesisToken;

    this(
        const(SyntaxToken) openParenthesisToken,
        const(ExpressionSyntax) expression,
        const(SyntaxToken) closeParenthesisToken,
    ) {
        _openParenthesisToken = openParenthesisToken;
        _expression = expression;
        _closeParenthesisToken = closeParenthesisToken;
    }

    const(SyntaxToken) openParenthesisToken() const @property {
        return _openParenthesisToken;
    }

    const(ExpressionSyntax) expression() const @property {
        return _expression;
    }

    const(SyntaxToken) closeParenthesisToken() const @property {
        return _closeParenthesisToken;
    }

    override SyntaxKind kind() const {
        return SyntaxKind.ParenthesizedExpression;
    }

    override const(SyntaxNode)[] children() const {
        return [
            cast(SyntaxNode) _openParenthesisToken,
            _expression,
            _closeParenthesisToken,
        ];
    }
}
