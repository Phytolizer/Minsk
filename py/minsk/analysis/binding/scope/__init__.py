from typing import Optional

from minsk.analysis.variable import VariableSymbol


class BoundScope:
    _variables: dict[str, VariableSymbol]
    parent: Optional["BoundScope"]

    def __init__(self, parent: Optional["BoundScope"]) -> None:
        self._variables = {}
        self.parent = parent

    def try_lookup(self, name: str) -> VariableSymbol | None:
        if name in self._variables:
            return self._variables[name]

        if self.parent is None:
            return None

        return self.parent.try_lookup(name)

    def try_declare(self, variable: VariableSymbol) -> bool:
        if variable.name in self._variables:
            return False

        self._variables[variable.name] = variable
        return True

    @property
    def declared_variables(self) -> tuple[VariableSymbol, ...]:
        return tuple(self._variables.values())
