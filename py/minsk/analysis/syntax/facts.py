from minsk.analysis.syntax.kind import SyntaxKind


def binary_operator_precedence(kind: SyntaxKind) -> int:
    match kind:
        case SyntaxKind.StarToken | SyntaxKind.SlashToken:
            return 4
        case SyntaxKind.PlusToken | SyntaxKind.MinusToken:
            return 3
        case SyntaxKind.AmpersandAmpersandToken:
            return 2
        case SyntaxKind.PipePipeToken:
            return 1
        case _:
            return 0


def unary_operator_precedence(kind: SyntaxKind) -> int:
    match kind:
        case SyntaxKind.PlusToken | SyntaxKind.MinusToken | SyntaxKind.BangToken:
            return 5
        case _:
            return 0


def keyword_kind(text: str) -> SyntaxKind:
    match text:
        case "true":
            return SyntaxKind.TrueKeyword
        case "false":
            return SyntaxKind.FalseKeyword
        case _:
            return SyntaxKind.IdentifierToken
