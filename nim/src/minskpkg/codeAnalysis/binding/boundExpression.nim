import minskpkg/minskObject

import boundNode

type
  BoundExpression* = ref object of BoundNode

method ty*(e: BoundExpression): MinskObjectKind {.base.} = discard
