module minsk.code_analysis.syntax.syntax_node;

import std.range : takeOne, front;
import std.stdio : write, writeln;

import minsk.code_analysis.syntax.syntax_kind : SyntaxKind;
import minsk.code_analysis.syntax.syntax_token : SyntaxToken;

interface SyntaxNode {
    SyntaxKind kind() const;
    const(SyntaxNode)[] children() const;

    static void prettyPrint(
        const(SyntaxNode) node,
        string indent = "",
        bool isLast = true,
    ) {
        write(indent);
        write(node.kind);
        if (auto t = cast(SyntaxToken) node)
            if (t.value) {
                write(" ");
                write(t.value);
            }
        writeln();

        indent ~= "    ";
        const children = node.children;
        const lastChild = node.children.takeOne;
        foreach (child; children) {
            prettyPrint(child, indent, child is lastChild.front);
        }
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
