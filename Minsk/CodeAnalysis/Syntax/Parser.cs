using System.Collections.Immutable;

namespace Minsk.CodeAnalysis.Syntax;

public class Parser
{
    private readonly ImmutableArray<SyntaxToken> _tokens;
    private int _position;
    private readonly List<string> _diagnostics;
    public IImmutableList<string> Diagnostics => _diagnostics.ToImmutableList();

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
        _diagnostics = lexer.Diagnostics.ToList();
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
        if (Current.Kind == kind)
        {
            return NextToken();
        }

        _diagnostics.Add($"Expected next token to be {kind}, got {Current.Kind} instead.");
        return new SyntaxToken(Current.Position, "", kind);
    }

    public SyntaxTree Parse()
    {
        var expression = ParseExpression();
        var endOfFileToken = MatchToken(SyntaxKind.EndOfFileToken);
        return new SyntaxTree(expression, endOfFileToken, _diagnostics.ToImmutableList());
    }

    private ExpressionSyntax ParseExpression()
    {
        return ParseBinaryExpression();
    }

    private ExpressionSyntax ParseBinaryExpression(int parentPrecedence = 0)
    {
        var unaryOperatorPrecedence = Current.Kind.GetUnaryOperatorPrecedence();
        ExpressionSyntax left;
        if (unaryOperatorPrecedence != 0 && unaryOperatorPrecedence >= parentPrecedence)
        {
            var operatorToken = NextToken();
            var operand = ParseBinaryExpression(unaryOperatorPrecedence);
            left = new UnaryExpressionSyntax(operatorToken, operand);
        }
        else
        {
            left = ParsePrimaryExpression();
        }

        while (true)
        {
            var precedence = Current.Kind.GetBinaryOperatorPrecedence();
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

        var literalToken = MatchToken(SyntaxKind.NumberToken);
        return new LiteralExpressionSyntax(literalToken);
    }
}