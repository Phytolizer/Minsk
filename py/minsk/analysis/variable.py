from dataclasses import dataclass

from minsk.analysis.type import MinskType


@dataclass(frozen=True)
class VariableSymbol:
    name: str
    ty: MinskType
