from typing import Any, Optional

from minsk.analysis.binding.binder import Binder
from minsk.analysis.diagnostic import Diagnostic
from minsk.analysis.evaluator import Evaluator
from minsk.analysis.syntax.parser import SyntaxTree
from minsk.analysis.variable import VariableSymbol

EvaluationResult = tuple[tuple[Diagnostic, ...], Optional[Any]]


class Compilation:
    syntax: SyntaxTree

    def __init__(self, syntax: SyntaxTree):
        self.syntax = syntax

    def evaluate(self, variables: dict[VariableSymbol, Any]) -> EvaluationResult:
        global_scope = Binder.bind_global_scope(self.syntax.root)
        diagnostics = self.syntax.diagnostics + global_scope.diagnostics
        if len(diagnostics) > 0:
            return diagnostics, None
        evaluator = Evaluator(global_scope.expression, variables)
        return (), evaluator.evaluate()
