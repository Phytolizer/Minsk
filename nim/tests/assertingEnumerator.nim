import std/algorithm
import std/options
import std/strutils

import minskpkg/codeAnalysis/syntax/[
  syntaxKind,
  syntaxNode,
  syntaxToken,
]

type
  Enumerator = object
    nodes: seq[SyntaxNode]
    i: int

  AssertingEnumerator* = object
    enumerator: Enumerator
    node: SyntaxNode
    failed: bool

proc newEnumerator(nodes: sink seq[SyntaxNode]): Enumerator =
  result.nodes = nodes
  result.i = 0

proc next(e: var Enumerator): Option[SyntaxNode] =
  for n in e.nodes:
    echo n.kind
  echo ""
  if e.i < e.nodes.len:
    result = some(e.nodes[e.i])
    e.i += 1

proc flatten(node: SyntaxNode): seq[SyntaxNode] =
  var stack: seq[SyntaxNode] = @[]
  stack.add(node)
  while stack.len > 0:
    let current = stack.pop()
    result.add(current)
    var children = current.children
    children.reverse()
    for child in children:
      stack.add(child)

proc newAssertingEnumerator*(node: SyntaxNode): AssertingEnumerator =
  result.enumerator = newEnumerator(flatten(node))
  result.node = node
  result.failed = false

proc assertToken*(e: var AssertingEnumerator, kind: SyntaxKind, text: string) =
  try:
    let n = e.enumerator.next().get
    assert ($n.kind).endsWith("Token")
    let token = n.SyntaxToken
    assert token.kind == kind
    assert token.text == text
  except AssertionDefect:
    e.failed = true
    raise

proc assertNode*(e: var AssertingEnumerator, kind: SyntaxKind) =
  try:
    let n = e.enumerator.next().get
    assert n isnot SyntaxToken
    assert n.kind == kind
  except AssertionDefect:
    e.failed = true
    raise

proc `=destroy`*(e: var AssertingEnumerator) =
  if not e.failed:
    assert e.enumerator.next().isNone
