from typing import Any

import pytest

from minsk.analysis.compilation import Compilation
from minsk.analysis.syntax.parser import SyntaxTree
from minsk.analysis.variable import VariableSymbol
from tests.analysis.text.annotated import AnnotatedText


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
        ("{ var a = 0 (a = 10) * a }", 100),
    ),
)
def test_evaluates_correct_value(text, expected):
    syntax_tree = SyntaxTree.parse(text)
    compilation = Compilation(syntax_tree)
    variables: dict[VariableSymbol, Any] = {}
    diagnostics, value = compilation.evaluate(variables)

    assert len(diagnostics) == 0
    assert value == expected


def assert_diagnostics(raw_text, raw_diagnostics):
    annotated_text = AnnotatedText.parse(raw_text)
    syntax_tree = SyntaxTree.parse(annotated_text.text)
    compilation = Compilation(syntax_tree)
    actual_diagnostics, _ = compilation.evaluate({})
    diagnostics = AnnotatedText.unindent_lines(raw_diagnostics)
    if len(diagnostics) != len(annotated_text.spans):
        raise ValueError("diagnostics do not match annotated text")

    assert len(actual_diagnostics) == len(diagnostics)

    for i in range(len(diagnostics)):
        assert actual_diagnostics[i].message == diagnostics[i]
        assert actual_diagnostics[i].span == annotated_text.spans[i]


def test_variable_expression_reports_undeclared():
    text = "[a] + 3"
    diagnostics = """
        Undefined name 'a'
    """
    assert_diagnostics(text, diagnostics)


def test_variable_declaration_reports_redeclaration():
    text = """
        {
            var x = 10
            var y = 20
            {
                var x = 30
            }
            var [x] = 40
        }
    """
    diagnostics = """
        Name 'x' is already declared in this scope
    """
    assert_diagnostics(text, diagnostics)


def test_binary_operator_reports_undefined():
    text = "3 [&&] 4"
    diagnostics = """
        The binary operator '&&' isn't defined for types 'integer' and 'integer'
    """
    assert_diagnostics(text, diagnostics)


def test_assignment_expression_reports_undefined():
    text = "[x] = 3"
    diagnostics = """
        Undefined name 'x'
    """
    assert_diagnostics(text, diagnostics)


def test_assignment_expression_reports_read_only():
    text = """
        {
            let x = 10
            x [=] 20
        }
    """
    diagnostics = """
        Cannot assign to read-only variable 'x'
    """
    assert_diagnostics(text, diagnostics)
