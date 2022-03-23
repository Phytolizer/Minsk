from typing import Iterable

from minsk.analysis.diagnostic import Diagnostic
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.text.span import TextSpan
from minsk.analysis.type import MinskType


class DiagnosticBag:
    _diagnostics: list[Diagnostic]

    def __init__(self):
        self._diagnostics = []

    def _report(self, span: TextSpan, message: str):
        self._diagnostics.append(Diagnostic(span, message))

    def report_bad_character(self, position: int, char: str):
        message = f"Bad character in input: '{char}'"
        self._report(TextSpan(position, 1), message)

    def report_invalid_int(self, span: TextSpan, text: str):
        message = f"Invalid integer: '{text}'"
        self._report(span, message)

    def report_unexpected_token(
        self, span: TextSpan, expected_kind: SyntaxKind, actual_kind: SyntaxKind
    ):
        message = (
            f"Expected next token to be {expected_kind.name}, "
            + f"got {actual_kind.name} instead"
        )
        self._report(span, message)

    def report_undefined_binary_operator(
        self, span: TextSpan, operator: str, left_type: MinskType, right_type: MinskType
    ):
        message = (
            f"The binary operator '{operator}' isn't defined for "
            + f"types {left_type} and {right_type}"
        )
        self._report(span, message)

    def report_undefined_unary_operator(
        self, span: TextSpan, operator: str, operand_type: MinskType
    ):
        message = (
            f"The unary operator '{operator}' isn't defined for "
            + f"type {operand_type}"
        )
        self._report(span, message)

    def report_undefined_name(self, span: TextSpan, name: str):
        message = f"Undefined name '{name}'"
        self._report(span, message)

    def report_variable_already_declared(self, span: TextSpan, name: str):
        message = f"Name '{name}' is already declared in this scope"
        self._report(span, message)

    def __iter__(self):
        return iter(self._diagnostics)

    def extend(self, diagnostics: Iterable[Diagnostic]):
        self._diagnostics.extend(diagnostics)
