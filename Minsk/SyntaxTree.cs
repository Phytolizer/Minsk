using System.Collections.Immutable;

namespace Minsk;

public record SyntaxTree(ExpressionSyntax Expression, SyntaxToken EndOfFileToken, ImmutableList<string> Diagnostics);