from dataclasses import dataclass
from typing import Optional

from minsk.analysis.binding.expression import BoundExpression
from minsk.analysis.diagnostic import Diagnostic
from minsk.analysis.variable import VariableSymbol


@dataclass(frozen=True)
class BoundGlobalScope:
    previous: Optional["BoundGlobalScope"]
    diagnostics: tuple[Diagnostic, ...]
    variables: tuple[VariableSymbol, ...]
    expression: BoundExpression
