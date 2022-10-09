proc assertSingle*[T](x: openArray[T]): T =
  assert x.len == 1
  x[0]
