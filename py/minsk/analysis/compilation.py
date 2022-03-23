from typing import Any, Optional

from minsk.analysis.binding.binder import Binder
from minsk.analysis.diagnostic import Diagnostic
from minsk.analysis.evaluator import Evaluator
from minsk.analysis.syntax.parser import SyntaxTree

EvaluationResult = tuple[tuple[Diagnostic, ...], Optional[Any]]


class Compilation:
    syntax: SyntaxTree

    def __init__(self, syntax: SyntaxTree):
        self.syntax = syntax

    def evaluate(self) -> EvaluationResult:
        binder = Binder()
        expression = binder.bind_expression(self.syntax.root)
        diagnostics = self.syntax.diagnostics + binder.diagnostics
        if len(diagnostics) > 0:
            return diagnostics, None
        evaluator = Evaluator(expression)
        return (), evaluator.evaluate()
