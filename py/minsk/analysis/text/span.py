from dataclasses import dataclass


@dataclass(frozen=True)
class TextSpan:
    start: int
    length: int

    @property
    def end(self) -> int:
        return self.start + self.length

    @staticmethod
    def from_bounds(start: int, end: int):
        return TextSpan(start, end - start)

    def __str__(self):
        return f"TextSpan(start: {self.start}, length: {self.length})"
