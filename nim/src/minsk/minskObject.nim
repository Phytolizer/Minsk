type
  MinskObjectKind* = enum
    mokNull,
    mokInteger,
    mokBoolean,
  MinskObject* = object
    case kind*: MinskObjectKind
    of mokInteger:
      intVal*: int
    of mokBoolean:
      boolVal*: bool
    else:
      discard

func moNull*: MinskObject =
  MinskObject(kind: mokNull)

func moInteger*(intVal: int): MinskObject =
  MinskObject(
    kind: mokInteger,
    intVal: intVal,
  )

func moBoolean*(boolVal: bool): MinskObject =
  MinskObject(
    kind: mokBoolean,
    boolVal: boolVal,
  )

func `$`*(kind: MinskObjectKind): string =
  case kind
  of mokNull: "null"
  of mokInteger: "integer"
  of mokBoolean: "boolean"

func `$`*(obj: MinskObject): string =
  case obj.kind
  of mokNull: "null"
  of mokInteger: $obj.intVal
  of mokBoolean: $obj.boolVal
