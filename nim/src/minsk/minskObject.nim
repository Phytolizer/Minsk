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

proc moNull*: MinskObject =
  MinskObject(kind: mokNull)

proc moInteger*(intVal: int): MinskObject =
  MinskObject(
    kind: mokInteger,
    intVal: intVal,
  )

proc moBoolean*(boolVal: bool): MinskObject =
  MinskObject(
    kind: mokBoolean,
    boolVal: boolVal,
  )

proc `$`*(kind: MinskObjectKind): string =
  case kind
  of mokNull: "null"
  of mokInteger: "integer"
  of mokBoolean: "boolean"

proc `$`*(obj: MinskObject): string =
  case obj.kind
  of mokNull: "null"
  of mokInteger: $obj.intVal
  of mokBoolean: $obj.boolVal
