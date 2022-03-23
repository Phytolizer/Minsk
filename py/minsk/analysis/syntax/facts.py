from typing import Iterable, Optional

from minsk.analysis.syntax.kind import SyntaxKind


def binary_operator_precedence(kind: SyntaxKind) -> int:
    match kind:
        case SyntaxKind.StarToken | SyntaxKind.SlashToken:
            return 5
        case SyntaxKind.PlusToken | SyntaxKind.MinusToken:
            return 4
        case SyntaxKind.BangEqualsToken | SyntaxKind.EqualsEqualsToken:
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
            return 6
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


def binary_operators() -> Iterable[SyntaxKind]:
    return filter(lambda kind: binary_operator_precedence(kind) > 0, SyntaxKind)


def unary_operators() -> Iterable[SyntaxKind]:
    return filter(lambda kind: unary_operator_precedence(kind) > 0, SyntaxKind)


def get_text(kind: SyntaxKind) -> Optional[str]:
    match kind:
        case SyntaxKind.PlusToken:
            return "+"
        case SyntaxKind.MinusToken:
            return "-"
        case SyntaxKind.StarToken:
            return "*"
        case SyntaxKind.SlashToken:
            return "/"
        case SyntaxKind.BangToken:
            return "!"
        case SyntaxKind.AmpersandAmpersandToken:
            return "&&"
        case SyntaxKind.PipePipeToken:
            return "||"
        case SyntaxKind.BangEqualsToken:
            return "!="
        case SyntaxKind.EqualsEqualsToken:
            return "=="
        case SyntaxKind.EqualsToken:
            return "="
        case SyntaxKind.OpenParenthesisToken:
            return "("
        case SyntaxKind.CloseParenthesisToken:
            return ")"
        case SyntaxKind.OpenBraceToken:
            return "{"
        case SyntaxKind.CloseBraceToken:
            return "}"
        case SyntaxKind.TrueKeyword:
            return "true"
        case SyntaxKind.FalseKeyword:
            return "false"
