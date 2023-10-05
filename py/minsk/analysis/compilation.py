import threading
from typing import Any, Optional

from minsk.analysis.binding.binder import Binder
from minsk.analysis.binding.scope.globl import BoundGlobalScope
from minsk.analysis.diagnostic import Diagnostic
from minsk.analysis.evaluator import Evaluator
from minsk.analysis.syntax.parser import SyntaxTree
from minsk.analysis.variable import VariableSymbol

EvaluationResult = tuple[tuple[Diagnostic, ...], Any | None]


class Compilation:
    syntax: SyntaxTree
    previous: Optional["Compilation"]
    _global_scope: BoundGlobalScope | None

    def __init__(
        self, syntax: SyntaxTree, previous: Optional["Compilation"] = None
    ) -> None:
        self.syntax = syntax
        self.previous = previous
        self._global_scope = None

    @property
    def global_scope(self) -> BoundGlobalScope:
        if self._global_scope is None:
            previous_scope = self.previous
            if previous_scope is not None:
                previous_scope = previous_scope.global_scope
            global_scope = Binder.bind_global_scope(previous_scope, self.syntax.root)
            with threading.Lock():
                if self._global_scope is None:
                    self._global_scope = global_scope

        return self._global_scope

    def continue_with(self, syntax_tree: SyntaxTree) -> "Compilation":
        return Compilation(syntax_tree, self)

    def evaluate(self, variables: dict[VariableSymbol, Any]) -> EvaluationResult:
        diagnostics = self.syntax.diagnostics + self.global_scope.diagnostics
        if len(diagnostics) > 0:
            return diagnostics, None
        evaluator = Evaluator(self.global_scope.statement, variables)
        return (), evaluator.evaluate()
