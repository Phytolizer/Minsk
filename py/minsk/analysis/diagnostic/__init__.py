from dataclasses import dataclass

from minsk.analysis.text.span import TextSpan


@dataclass(frozen=True)
class Diagnostic:
    span: TextSpan
    message: str

    def __str__(self):
        return self.message
