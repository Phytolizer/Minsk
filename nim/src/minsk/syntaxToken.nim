import syntaxKind
import syntaxNode
import minskObject

type
  SyntaxToken* = ref object of SyntaxNode
    pKind: SyntaxKind
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
  result.pKind = kind
  result.position = position
  result.text = text
  result.value = value

method kind*(self: SyntaxToken): SyntaxKind =
  self.pKind

method children*(self: SyntaxToken): seq[SyntaxNode] =
  @[]
