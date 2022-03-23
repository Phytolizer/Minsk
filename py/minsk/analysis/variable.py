from dataclasses import dataclass
from typing import Type


@dataclass
class VariableSymbol:
    name: str
    ty: Type
