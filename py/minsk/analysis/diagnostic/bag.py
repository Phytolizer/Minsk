from collections.abc import Iterable, Iterator

from minsk.analysis.diagnostic import Diagnostic
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.text.span import TextSpan
from minsk.analysis.type import MinskType


class DiagnosticBag:
    _diagnostics: list[Diagnostic]

    def __init__(self) -> None:
        self._diagnostics = []

    def _report(self, span: TextSpan, message: str) -> None:
        self._diagnostics.append(Diagnostic(span, message))

    def report_bad_character(self, position: int, char: str) -> None:
        message = f"Bad character in input: '{char}'"
        self._report(TextSpan(position, 1), message)

    def report_invalid_int(self, span: TextSpan, text: str) -> None:
        message = f"Invalid integer: '{text}'"
        self._report(span, message)

    def report_unexpected_token(
        self, span: TextSpan, expected_kind: SyntaxKind, actual_kind: SyntaxKind
    ) -> None:
        message = (
            f"Expected next token to be {expected_kind.name}, "
            f"got {actual_kind.name} instead"
        )
        self._report(span, message)

    def report_undefined_binary_operator(
        self, span: TextSpan, operator: str, left_type: MinskType, right_type: MinskType
    ) -> None:
        message = (
            f"The binary operator '{operator}' isn't defined for "
            f"types '{left_type}' and '{right_type}'"
        )
        self._report(span, message)

    def report_undefined_unary_operator(
        self, span: TextSpan, operator: str, operand_type: MinskType
    ) -> None:
        message = (
            f"The unary operator '{operator}' isn't defined for type {operand_type}"
        )
        self._report(span, message)

    def report_undefined_name(self, span: TextSpan, name: str) -> None:
        message = f"Undefined name '{name}'"
        self._report(span, message)

    def report_variable_already_declared(self, span: TextSpan, name: str) -> None:
        message = f"Name '{name}' is already declared in this scope"
        self._report(span, message)

    def report_cannot_convert(
        self, span: TextSpan, from_type: MinskType, to_type: MinskType
    ) -> None:
        message = f"Cannot convert type {from_type} to type {to_type}"
        self._report(span, message)

    def report_cannot_assign(self, span: TextSpan, name: str) -> None:
        message = f"Cannot assign to read-only variable '{name}'"
        self._report(span, message)

    def __iter__(self) -> Iterator[Diagnostic]:
        return iter(self._diagnostics)

    def extend(self, diagnostics: Iterable[Diagnostic]) -> None:
        self._diagnostics.extend(diagnostics)
