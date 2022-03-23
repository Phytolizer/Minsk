from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.lexer import Lexer
from minsk.analysis.syntax.token import SyntaxToken


class Parser:
    _tokens: tuple[SyntaxToken, ...]
    _position: int

    def __init__(self, text: str):
        lexer = Lexer(text)
        tokens = list(
            filter(
                lambda tok: tok.kind != SyntaxKind.BadToken and
                            tok.kind != SyntaxKind.WhitespaceToken,
                lexer
            )
        )
        tokens.append(SyntaxToken(SyntaxKind.EndOfFileToken, 0, "", None))
        self._tokens = tuple(tokens)
        self._position = 0

    def _peek(self, offset: int) -> SyntaxToken:
        index = self._position + offset
        if index >= len(self._tokens):
            return self._tokens[-1]
        return self._tokens[index]

    @property
    def _current(self) -> SyntaxToken:
        return self._peek(0)

    def parse(self) -> ExpressionSyntax:
        pass
