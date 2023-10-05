import pytest

from minsk.analysis.text.source import SourceText


@pytest.mark.parametrize(
    "text,expected_line_count",
    (
        (".", 1),
        (".\r\n", 2),
        (".\r\n\r\n", 3),
    ),
)
def test_includes_last_line(text: str, expected_line_count: int) -> None:
    source_text = SourceText(text)
    assert len(source_text.lines) == expected_line_count
