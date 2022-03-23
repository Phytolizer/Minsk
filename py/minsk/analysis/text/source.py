from dataclasses import dataclass

from minsk.analysis.text.span import TextSpan


@dataclass(frozen=True)
class TextLine:
    _text: "SourceText"
    start: int
    length: int
    length_including_line_break: int

    @property
    def end(self) -> int:
        return self.start + self.length

    @property
    def end_including_line_break(self) -> int:
        return self.start + self.length_including_line_break

    @property
    def span(self) -> TextSpan:
        return TextSpan(self.start, self.length)

    @property
    def span_including_line_break(self) -> TextSpan:
        return TextSpan(self.start, self.length_including_line_break)


class SourceText:
    _text: str
    lines: tuple[TextLine, ...]

    def __init__(self, text: str):
        self._text = text
        self.lines = self._parse_lines(text)

    def _parse_lines(self, text: str) -> tuple[TextLine, ...]:
        result = []

        position = 0
        line_start = 0

        while position < len(text):
            line_break_width = self._get_line_break_width(text, position)

            if line_break_width > 0:
                self._add_line(result, line_start, position, line_break_width)
                position += line_break_width
                line_start = position
            else:
                position += 1

        self._add_line(result, line_start, position, 0)

        return tuple(result)

    @staticmethod
    def _get_line_break_width(text: str, position: int) -> int:
        look = text[position + 1] if position + 1 < len(text) else "\0"
        match text[position]:
            case "\r" if look == "\n":
                return 2
            case "\r" | "\n":
                return 1
            case _:
                return 0

    def _add_line(
        self,
        result: list[TextLine],
        line_start: int,
        position: int,
        line_break_width: int,
    ):
        length = position - line_start
        length_including_line_break = length + line_break_width
        line = TextLine(self, line_start, length, length_including_line_break)
        result.append(line)

    def get_line_index(self, position: int) -> int:
        lower = 0
        upper = len(self._text) - 1

        while lower <= upper:
            index = lower + (upper - lower) // 2
            start = self.lines[index].start

            if position == start:
                return index

            if start > position:
                upper = index - 1
            else:
                lower = index + 1

        return lower + 1

    def __str__(self):
        return self._text

    def to_str(self, span: TextSpan) -> str:
        return self._text[span.start : span.end]

    def __len__(self):
        return len(self._text)

    def __getitem__(self, item):
        return self._text[item]
