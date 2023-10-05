from enum import Enum, auto


class MinskType(Enum):
    Int = auto()
    Bool = auto()

    def __str__(self) -> str:
        match self:
            case MinskType.Int:
                return "integer"
            case MinskType.Bool:
                return "boolean"
