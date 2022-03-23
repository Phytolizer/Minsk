from minsk.analysis.syntax.kind import SyntaxKind


def binary_operator_precedence(kind: SyntaxKind) -> int:
    match kind:
        case SyntaxKind.StarToken | SyntaxKind.SlashToken:
            return 2
        case SyntaxKind.PlusToken | SyntaxKind.MinusToken:
            return 1
        case _:
            return 0


def unary_operator_precedence(kind: SyntaxKind) -> int:
    match kind:
        case SyntaxKind.PlusToken | SyntaxKind.MinusToken:
            return 3
        case _:
            return 0
