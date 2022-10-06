import std/sequtils
import std/tables

import minsk/minskObject

import variableSymbol

type
  VariableMap* = object
    inner: TableRef[VariableSymbol, MinskObject]

proc newVariableMap*(): VariableMap =
  result.inner = newTable[VariableSymbol, MinskObject]()

proc `[]=`*(self: var VariableMap, key: VariableSymbol, value: MinskObject) =
  self.inner[key] = value

proc `[]`*(self: VariableMap, key: VariableSymbol): MinskObject =
  self.inner[key]

proc hasKey*(self: VariableMap, key: VariableSymbol): bool =
  self.inner.hasKey(key)

proc del*(self: var VariableMap, key: VariableSymbol) =
  self.inner.del(key)

proc keys*(self: VariableMap): seq[VariableSymbol] =
  self.inner.keys.toSeq
