import syntaxKind
import minskObject

type
  SyntaxToken* = object
    kind*: SyntaxKind
    position*: int
    text*: string
    value*: MinskObject
