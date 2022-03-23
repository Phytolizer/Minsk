from minsk.analysis.syntax import facts
from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.expressions.binary import BinaryExpressionSyntax
from minsk.analysis.syntax.expressions.literal import LiteralExpressionSyntax
from minsk.analysis.syntax.expressions.parenthesized import (
    ParenthesizedExpressionSyntax,
)
from minsk.analysis.syntax.expressions.unary import UnaryExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.lexer import Lexer
from minsk.analysis.syntax.token import SyntaxToken
from minsk.analysis.syntax.tree import SyntaxTree


class Parser:
    _tokens: tuple[SyntaxToken, ...]
    _position: int
    _diagnostics: list[str]

    def __init__(self, text: str):
        lexer = Lexer(text)
        tokens = list(
            filter(
                lambda tok: tok.kind != SyntaxKind.BadToken
                and tok.kind != SyntaxKind.WhitespaceToken,
                lexer,
            )
        )
        tokens.append(SyntaxToken(SyntaxKind.EndOfFileToken, 0, "", None))
        self._tokens = tuple(tokens)
        self._position = 0
        self._diagnostics = lexer.diagnostics

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
        self._diagnostics.append(
            f"Expected next token to be {kind.name}, not {self._current.kind.name}"
        )
        return SyntaxToken(kind, self._current.position, "", None)

    def parse(self) -> SyntaxTree:
        return SyntaxTree(
            self._parse_expression(),
            self._match_token(SyntaxKind.EndOfFileToken),
            self.diagnostics,
        )

    def _parse_expression(self) -> ExpressionSyntax:
        return self._parse_binary_expression(0)

    def _parse_binary_expression(self, parent_precedence: int) -> ExpressionSyntax:
        unary_operator_precedence = facts.unary_operator_precedence(self._current.kind)
        if (
            unary_operator_precedence != 0
            and unary_operator_precedence >= parent_precedence
        ):
            operator_token = self._next_token()
            operand = self._parse_binary_expression(unary_operator_precedence)
            left = UnaryExpressionSyntax(operator_token, operand)
        else:
            left = self._parse_primary_expression()

        while True:
            precedence = facts.binary_operator_precedence(self._current.kind)
            if precedence == 0 or precedence <= parent_precedence:
                break

            operator_token = self._next_token()
            right = self._parse_binary_expression(precedence)
            left = BinaryExpressionSyntax(left, operator_token, right)

        return left

    def _parse_primary_expression(self) -> ExpressionSyntax:
        match self._current.kind:
            case SyntaxKind.OpenParenthesisToken:
                return self._parse_parenthesized_expression()
            case _:
                return self._parse_number_literal()

    def _parse_number_literal(self) -> ExpressionSyntax:
        number_token = self._match_token(SyntaxKind.NumberToken)
        return LiteralExpressionSyntax(number_token)

    @property
    def diagnostics(self) -> tuple[str, ...]:
        return tuple(iter(self._diagnostics))

    def _parse_parenthesized_expression(self) -> ExpressionSyntax:
        open_parenthesis_token = self._match_token(SyntaxKind.OpenParenthesisToken)
        expression = self._parse_expression()
        close_parenthesis_token = self._match_token(SyntaxKind.CloseParenthesisToken)
        return ParenthesizedExpressionSyntax(
            open_parenthesis_token, expression, close_parenthesis_token
        )
