using System.Collections.Immutable;

namespace Minsk;

public class Parser
{
    private readonly ImmutableArray<SyntaxToken> _tokens;
    private List<string> _diagnostics;
    private int _position;

    public IEnumerable<string> Diagnostics => _diagnostics;

    public Parser(string text)
    {
        var lexer = new Lexer(text);
        _tokens = lexer
            .Select(tok => tok!)
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

    public ExpressionSyntax Parse()
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
        var numberToken = MatchToken(SyntaxKind.NumberToken);
        return new LiteralExpressionSyntax(numberToken);
    }
}