from typing import Any

import pytest

from minsk.analysis.compilation import Compilation
from minsk.analysis.syntax.parser import SyntaxTree
from minsk.analysis.variable import VariableSymbol


@pytest.mark.parametrize(
    "text,expected",
    (
        ("1", 1),
        ("+1", 1),
        ("-1", -1),
        ("14 + 12", 26),
        ("12 - 3", 9),
        ("4 * 2", 8),
        ("9 / 3", 3),
        ("(10)", 10),
        ("12 == 3", False),
        ("3 == 3", True),
        ("12 != 3", True),
        ("3 != 3", False),
        ("false == false", True),
        ("true == false", False),
        ("false != false", False),
        ("true != false", True),
        ("true", True),
        ("false", False),
        ("!true", False),
        ("!false", True),
        ("(a = 10) * a", 100),
    ),
)
def test_evaluates_correct_value(text, expected):
    syntax_tree = SyntaxTree.parse(text)
    compilation = Compilation(syntax_tree)
    variables: dict[VariableSymbol, Any] = {}
    diagnostics, value = compilation.evaluate(variables)

    assert len(diagnostics) == 0
    assert value == expected
