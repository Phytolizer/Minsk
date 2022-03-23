from dataclasses import dataclass

from minsk.analysis.type import MinskType


@dataclass(frozen=True)
class VariableSymbol:
    name: str
    is_read_only: bool
    ty: MinskType
