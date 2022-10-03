import minsk/minskObject

import boundNode

type
  BoundExpression* = ref object of BoundNode

method objectKind*(e: BoundExpression): MinskObjectKind {.base.} = discard
