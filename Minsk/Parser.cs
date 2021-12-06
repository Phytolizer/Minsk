using System.Collections.Immutable;

namespace Minsk;

public class Parser
{
    private readonly ImmutableArray<SyntaxToken> _tokens;
    private int _position = 0;

    public Parser(string text)
    {
        var lexer = new Lexer(text);
        var tokens = new List<SyntaxToken>();
        while (true)
        {
            var token = lexer.NextToken();
            if (token.Kind != SyntaxKind.WhitespaceToken && token.Kind != SyntaxKind.BadToken)
            {
                tokens.Add(token);
            }

            if (token.Kind == SyntaxKind.EndOfFileToken)
            {
                break;
            }
        }

        _tokens = tokens.ToImmutableArray();
    }

    private SyntaxToken Peek(int offset)
    {
        var index = _position + offset;

        return index >= _tokens.Length ? _tokens.Last() : _tokens[index];
    }

    private SyntaxToken Current => Peek(0);

    private SyntaxToken NextToken()
    {
        var current = Current;
        ++_position;
        return current;
    }

    private SyntaxToken MatchToken(SyntaxKind kind)
    {
        return Current.Kind == kind ? NextToken() : new SyntaxToken(Current.Position, "", kind);
    }

    public ExpressionSyntax ParseExpression()
    {
        return ParseBinaryExpression();
    }

    private ExpressionSyntax ParseBinaryExpression()
    {
        var left = ParsePrimaryExpression();
        while (Current.Kind is SyntaxKind.PlusToken or SyntaxKind.MinusToken)
        {
            var operatorToken = NextToken();
            var right = ParsePrimaryExpression();
            left = new BinaryExpressionSyntax(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionSyntax ParsePrimaryExpression()
    {
        var literalToken = MatchToken(SyntaxKind.NumberToken);
        return new LiteralExpressionSyntax(literalToken);
    }
}