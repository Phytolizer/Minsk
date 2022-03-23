from typing import Optional

from minsk.analysis.variable import VariableSymbol


class BoundScope:
    _variables: dict[str, VariableSymbol]
    _parent: Optional["BoundScope"]

    def __init__(self, parent: Optional["BoundScope"]):
        self._variables = {}
        self._parent = parent

    def try_lookup(self, name: str) -> Optional[VariableSymbol]:
        if name in self._variables.keys():
            return self._variables[name]

        if self._parent is None:
            return None

        return self._parent.try_lookup(name)

    def try_declare(self, variable: VariableSymbol) -> bool:
        if variable.name in self._variables.keys():
            return False

        self._variables[variable.name] = variable
        return True

    @property
    def declared_variables(self) -> tuple[VariableSymbol, ...]:
        return tuple(self._variables.values())
