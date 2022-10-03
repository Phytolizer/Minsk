import minsk/minskObject

import syntaxKind
import syntaxNode

type
  SyntaxToken* = ref object of SyntaxNode
    mKind: SyntaxKind
    position*: int
    text*: string
    value*: MinskObject

proc newToken*(
  kind: SyntaxKind,
  position: int,
  text: string,
  value: MinskObject
): SyntaxToken =
  new(result)
  result.mKind = kind
  result.position = position
  result.text = text
  result.value = value

method kind*(self: SyntaxToken): SyntaxKind =
  self.mKind

method children*(self: SyntaxToken): seq[SyntaxNode] =
  @[]
