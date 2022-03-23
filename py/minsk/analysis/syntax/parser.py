from dataclasses import dataclass

from minsk.analysis.diagnostic import Diagnostic
from minsk.analysis.diagnostic.bag import DiagnosticBag
from minsk.analysis.syntax import facts
from minsk.analysis.syntax.expression import ExpressionSyntax
from minsk.analysis.syntax.expressions.assignment import AssignmentExpressionSyntax
from minsk.analysis.syntax.expressions.binary import BinaryExpressionSyntax
from minsk.analysis.syntax.expressions.literal import LiteralExpressionSyntax
from minsk.analysis.syntax.expressions.name import NameExpressionSyntax
from minsk.analysis.syntax.expressions.parenthesized import (
    ParenthesizedExpressionSyntax,
)
from minsk.analysis.syntax.expressions.unary import UnaryExpressionSyntax
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.lexer import Lexer
from minsk.analysis.syntax.statement import StatementSyntax
from minsk.analysis.syntax.statements.block import BlockStatementSyntax
from minsk.analysis.syntax.statements.expression import ExpressionStatementSyntax
from minsk.analysis.syntax.token import SyntaxToken
from minsk.analysis.syntax.unit import CompilationUnitSyntax
from minsk.analysis.text.source import SourceText


class Parser:
    _text: SourceText
    _tokens: tuple[SyntaxToken, ...]
    _position: int
    _diagnostics: DiagnosticBag

    def __init__(self, text: SourceText):
        self._text = text
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
        self._diagnostics = DiagnosticBag()
        self._diagnostics.extend(lexer.diagnostics)

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
        self._diagnostics.report_unexpected_token(
            self._current.span, kind, self._current.kind
        )
        return SyntaxToken(kind, self._current.position, "", None)

    def parse_compilation_unit(self) -> CompilationUnitSyntax:
        return CompilationUnitSyntax(
            self._parse_statement(),
            self._match_token(SyntaxKind.EndOfFileToken),
        )

    def _parse_statement(self) -> StatementSyntax:
        match self._current.kind:
            case SyntaxKind.OpenBraceToken:
                return self._parse_block_statement()
            case _:
                return self._parse_expression_statement()

    def _parse_block_statement(self) -> StatementSyntax:
        open_brace_token = self._match_token(SyntaxKind.OpenBraceToken)
        statements: list[StatementSyntax] = []
        while self._current.kind not in (
            SyntaxKind.CloseBraceToken,
            SyntaxKind.EndOfFileToken,
        ):
            statements.append(self._parse_statement())
        close_brace_token = self._match_token(SyntaxKind.CloseBraceToken)
        return BlockStatementSyntax(
            open_brace_token, tuple(statements), close_brace_token
        )

    def _parse_expression_statement(self) -> StatementSyntax:
        expression = self._parse_expression()
        return ExpressionStatementSyntax(expression)

    def _parse_expression(self) -> ExpressionSyntax:
        return self._parse_assignment_expression()

    def _parse_assignment_expression(self) -> ExpressionSyntax:
        if (
            self._peek(0).kind != SyntaxKind.IdentifierToken
            or self._peek(1).kind != SyntaxKind.EqualsToken
        ):
            return self._parse_binary_expression(0)

        identifier_token = self._next_token()
        equals_token = self._next_token()
        expression = self._parse_assignment_expression()
        return AssignmentExpressionSyntax(identifier_token, equals_token, expression)

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
            case SyntaxKind.TrueKeyword | SyntaxKind.FalseKeyword:
                return self._parse_boolean_literal()
            case SyntaxKind.NumberToken:
                return self._parse_number_literal()
            case _:
                return self._parse_name_expression()

    def _parse_number_literal(self) -> ExpressionSyntax:
        number_token = self._match_token(SyntaxKind.NumberToken)
        return LiteralExpressionSyntax(number_token, number_token.value)

    @property
    def diagnostics(self) -> tuple[Diagnostic, ...]:
        return tuple(iter(self._diagnostics))

    def _parse_parenthesized_expression(self) -> ExpressionSyntax:
        open_parenthesis_token = self._match_token(SyntaxKind.OpenParenthesisToken)
        expression = self._parse_expression()
        close_parenthesis_token = self._match_token(SyntaxKind.CloseParenthesisToken)
        return ParenthesizedExpressionSyntax(
            open_parenthesis_token, expression, close_parenthesis_token
        )

    def _parse_boolean_literal(self) -> ExpressionSyntax:
        is_true = self._current.kind == SyntaxKind.TrueKeyword
        keyword_token = self._match_token(
            SyntaxKind.TrueKeyword if is_true else SyntaxKind.FalseKeyword
        )
        return LiteralExpressionSyntax(keyword_token, is_true)

    def _parse_name_expression(self) -> ExpressionSyntax:
        identifier_token = self._match_token(SyntaxKind.IdentifierToken)
        return NameExpressionSyntax(identifier_token)


@dataclass(init=False)
class SyntaxTree:
    text: SourceText
    root: CompilationUnitSyntax
    diagnostics: tuple[Diagnostic, ...]

    def __init__(self, text: SourceText):
        parser = Parser(text)
        root = parser.parse_compilation_unit()
        diagnostics = parser.diagnostics

        self.text = text
        self.root = root
        self.diagnostics = diagnostics

    @staticmethod
    def parse(text: str | SourceText) -> "SyntaxTree":
        if isinstance(text, str):
            text = SourceText(text)
        return SyntaxTree(text)

    @staticmethod
    def parse_tokens(text: str | SourceText) -> tuple[SyntaxToken]:
        if isinstance(text, str):
            text = SourceText(text)
        return tuple(Lexer(text))
