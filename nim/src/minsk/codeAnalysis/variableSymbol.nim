import minsk/minskObject

type
  VariableSymbol* = object
    name*: string
    ty*: MinskObjectKind

proc newVariableSymbol*(name: string, ty: MinskObjectKind): VariableSymbol =
  result.name = name
  result.ty = ty
