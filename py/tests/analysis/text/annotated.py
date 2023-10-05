from minsk.analysis.text.span import TextSpan


class AnnotatedText:
    text: str
    spans: list[TextSpan]

    def __init__(self, text: str, spans: list[TextSpan]) -> None:
        self.text = text
        self.spans = spans

    @staticmethod
    def parse(text: str) -> "AnnotatedText":
        text = AnnotatedText._unindent(text)
        stripped_text = []
        spans = []
        stack = []
        position = 0
        for c in text:
            match c:
                case "[":
                    stack.append(position)
                case "]":
                    start = stack.pop()
                    end = position
                    spans.append(TextSpan.from_bounds(start, end))
                case c:
                    position += 1
                    stripped_text.append(c)

        if len(stack) > 0:
            raise ValueError("too many '[' in annotated text")

        return AnnotatedText("".join(stripped_text), spans)

    @staticmethod
    def _unindent(text: str) -> str:
        return "\n".join(AnnotatedText.unindent_lines(text))

    @staticmethod
    def unindent_lines(text: str) -> list[str]:
        lines = text.split("\n")
        line_with_min_indent = min(
            filter(lambda line: len(line.strip()) > 0, lines),
            key=lambda line: len(line) - len(line.lstrip()),
        )
        min_indent = len(line_with_min_indent) - len(line_with_min_indent.lstrip())
        for i in range(len(lines)):
            if len(lines[i].lstrip()) == 0:
                continue
            lines[i] = lines[i][min_indent:]

        while len(lines) > 0 and len(lines[0].lstrip()) == 0:
            lines = lines[1:]

        while len(lines) > 0 and len(lines[-1].lstrip()) == 0:
            lines = lines[:-1]

        return lines
