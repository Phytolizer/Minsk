using System.Collections.Immutable;

namespace Minsk;

public record SyntaxTree(ExpressionSyntax Expression, ImmutableList<string> Diagnostics);