type
  MinskObjectKind* = enum
    mokNull,
    mokInteger,
  MinskObject* = object
    case kind*: MinskObjectKind
    of mokInteger:
      intVal*: int
    else:
      discard

func moNull*: MinskObject =
  MinskObject(kind: mokNull)

func moInteger*(intVal: int): MinskObject =
  MinskObject(
    kind: mokInteger,
    intVal: intVal,
  )
