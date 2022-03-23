from typing import Any, Optional

from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.token import SyntaxToken


class Lexer:
    class _Iter:
        _text: str
        _position: int

        def __init__(self, text):
            self._text = text
            self._position = 0

        @property
        def _current(self) -> str:
            return self._peek(0)

        def __next__(self):
            if self._position >= len(self._text):
                raise StopIteration

            start = self._position
            kind = SyntaxKind.BadToken
            value: Optional[Any] = None
            current_text: Optional[str] = None

            match self._current:
                case c if c.isspace():
                    while self._current.isspace():
                        self._position += 1
                    kind = SyntaxKind.WhitespaceToken
                case c if c.isdigit():
                    while self._current.isdigit():
                        self._position += 1
                    kind = SyntaxKind.NumberToken
                    current_text = self._current_text(start)
                    try:
                        value = int(current_text)
                    except ValueError:
                        pass
                case "+":
                    kind = SyntaxKind.PlusToken
                    self._position += 1
                case "-":
                    kind = SyntaxKind.MinusToken
                    self._position += 1
                case "*":
                    kind = SyntaxKind.StarToken
                    self._position += 1
                case "/":
                    kind = SyntaxKind.SlashToken
                    self._position += 1
                case "(":
                    kind = SyntaxKind.OpenParenthesisToken
                    self._position += 1
                case ")":
                    kind = SyntaxKind.CloseParenthesisToken
                    self._position += 1

            if kind == SyntaxKind.BadToken:
                self._position += 1

            if current_text is None:
                current_text = self._current_text(start)

            return SyntaxToken(kind, start, current_text, value)

        def _peek(self, offset: int) -> str:
            index = self._position + offset
            if index >= len(self._text):
                return "\0"
            return self._text[index]

        def _current_text(self, start: int) -> str:
            return self._text[start:self._position]

    _text: str

    def __init__(self, text: str):
        self._text = text

    def __iter__(self):
        return Lexer._Iter(self._text)
