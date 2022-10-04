import std/options

proc find*[T](s: openArray[T], predicate: proc(x: T): bool): Option[T] =
  for x in s:
    if predicate(x): return some(x)
  return none(T)
