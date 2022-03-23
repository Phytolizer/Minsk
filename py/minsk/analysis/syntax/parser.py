from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.expressions.binary import BinaryExpressionSyntax
from minsk.analysis.syntax.expressions.literal import LiteralExpressionSyntax
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

    def _next_token(self) -> SyntaxToken:
        current = self._current
        self._position += 1
        return current

    def _match_token(self, kind: SyntaxKind) -> SyntaxToken:
        if self._current.kind == kind:
            return self._next_token()
        return SyntaxToken(kind, self._current.position, "", None)

    def parse(self) -> ExpressionSyntax:
        return self._parse_expression()

    def _parse_expression(self) -> ExpressionSyntax:
        return self._parse_term()

    def _parse_term(self) -> ExpressionSyntax:
        left = self._parse_factor()

        while self._current.kind in (SyntaxKind.PlusToken, SyntaxKind.MinusToken):
            operator_token = self._next_token()
            right = self._parse_factor()
            left = BinaryExpressionSyntax(left, operator_token, right)

        return left

    def _parse_factor(self) -> ExpressionSyntax:
        left = self._parse_primary_expression()

        while self._current.kind in (SyntaxKind.StarToken, SyntaxKind.SlashToken):
            operator_token = self._next_token()
            right = self._parse_primary_expression()
            left = BinaryExpressionSyntax(left, operator_token, right)

        return left

    def _parse_primary_expression(self) -> ExpressionSyntax:
        number_token = self._match_token(SyntaxKind.NumberToken)
        return LiteralExpressionSyntax(number_token)
