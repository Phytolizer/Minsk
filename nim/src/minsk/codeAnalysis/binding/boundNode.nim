import boundNodeKind

type
  BoundNode* = ref object of RootObj

method kind*(n: BoundNode): BoundNodeKind {.base.} = discard
