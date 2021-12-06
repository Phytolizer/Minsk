namespace Minsk;

public record SyntaxToken(int Position, string Text, SyntaxKind Kind, object? Value = null);