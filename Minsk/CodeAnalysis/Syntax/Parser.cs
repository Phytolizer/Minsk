using System.Collections.Immutable;

namespace Minsk.CodeAnalysis.Syntax;

public sealed class Parser
{
    private readonly ImmutableArray<SyntaxToken> _tokens;
    private readonly List<string> _diagnostics;
    private int _position;

    public Parser(string text)
    {
        var lexer = new Lexer(text);
        _tokens = lexer
            .Where(tok => tok.Kind != SyntaxKind.BadToken && tok.Kind != SyntaxKind.WhitespaceToken)
            .ToImmutableArray();
        _diagnostics = lexer.Diagnostics.ToList();
    }

    private SyntaxToken Peek(int offset)
    {
        var index = _position + offset;
        if (index < _tokens.Length)
        {
            return _tokens[index];
        }

        var lastPosition = 0;
        if (_tokens.Length > 0)
        {
            lastPosition = _tokens.Last().Position;
        }

        return new SyntaxToken(SyntaxKind.EndOfFileToken, "", lastPosition, null);
    }

    private SyntaxToken Current => Peek(0);

    private SyntaxToken NextToken()
    {
        var current = Current;
        _position++;
        return current;
    }

    private SyntaxToken MatchToken(SyntaxKind kind)
    {
        if (Current is { } token && token.Kind == kind)
        {
            return NextToken();
        }

        _diagnostics.Add($"Expected next token to be {kind}, got {Current.Kind} instead.");
        return new SyntaxToken(kind, "", Current.Position, null);
    }

    public SyntaxTree Parse()
    {
        return new SyntaxTree(ParseExpression(), MatchToken(SyntaxKind.EndOfFileToken), _diagnostics.ToArray());
    }

    private ExpressionSyntax ParseExpression()
    {
        return ParseBinaryExpression(0);
    }

    private ExpressionSyntax ParseBinaryExpression(int parentPrecedence)
    {
        ExpressionSyntax left;
        var unaryOperatorPrecedence = SyntaxFacts.GetUnaryOperatorPrecedence(Current.Kind);
        if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence)
        {
            var operatorToken = NextToken();
            var right = ParseBinaryExpression(unaryOperatorPrecedence);
            left = new UnaryExpressionSyntax(operatorToken, right);
        }
        else
        {
            left = ParsePrimaryExpression();
        }

        while (true)
        {
            var precedence = SyntaxFacts.GetBinaryOperatorPrecedence(Current.Kind);
            if (precedence == 0 || precedence <= parentPrecedence)
            {
                break;
            }

            var operatorToken = NextToken();
            var right = ParseBinaryExpression(precedence);
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax ParsePrimaryExpression()
    {
        if (Current.Kind == SyntaxKind.OpenParenthesisToken)
        {
            var openParenthesisToken = MatchToken(SyntaxKind.OpenParenthesisToken);
            var expression = ParseExpression();
            var closeParenthesisToken = MatchToken(SyntaxKind.CloseParenthesisToken);
            return new ParenthesizedExpressionSyntax(openParenthesisToken, expression, closeParenthesisToken);
        }

        var numberToken = MatchToken(SyntaxKind.NumberToken);
        return new LiteralExpressionSyntax(numberToken);
    }
}