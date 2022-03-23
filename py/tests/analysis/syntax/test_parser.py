from typing import Iterable, Iterator

import pytest

from minsk.analysis.syntax import facts
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.node import SyntaxNode
from minsk.analysis.syntax.parser import SyntaxTree
from minsk.analysis.syntax.token import SyntaxToken


class AssertingIterator:
    _iter: Iterator[SyntaxNode]

    def __init__(self, root: SyntaxNode):
        self._iter = self._flatten(root)

    @staticmethod
    def _flatten(root: SyntaxNode) -> Iterator[SyntaxNode]:
        stack: list[SyntaxNode] = [root]

        while len(stack) > 0:
            node = stack.pop()
            yield node

            for child in reversed(list(node.children)):
                stack.append(child)

    def assert_node(self, kind: SyntaxKind):
        node = next(self._iter)
        assert node.kind == kind
        assert not isinstance(node, SyntaxToken)

    def assert_token(self, kind: SyntaxKind, text: str):
        token = next(self._iter)
        assert token.kind == kind
        assert isinstance(token, SyntaxToken)
        assert token.text == text

    def assert_at_end(self):
        try:
            next(self._iter)
            assert False, "not at end"
        except StopIteration:
            pass


def get_binary_operator_pairs() -> Iterable[tuple[SyntaxKind, SyntaxKind]]:
    for op1 in facts.binary_operators():
        for op2 in facts.binary_operators():
            yield op1, op2


@pytest.mark.parametrize("op1,op2", get_binary_operator_pairs())
def test_binary_operator_precedence(op1: SyntaxKind, op2: SyntaxKind):
    op1_text = facts.get_text(op1)
    op2_text = facts.get_text(op2)
    op1_precedence = facts.binary_operator_precedence(op1)
    op2_precedence = facts.binary_operator_precedence(op2)

    text = f"a {op1_text} b {op2_text} c"
    expression = SyntaxTree.parse(text).root

    if op1_precedence >= op2_precedence:
        i = AssertingIterator(expression)

        i.assert_node(SyntaxKind.BinaryExpression)
        i.assert_node(SyntaxKind.BinaryExpression)
        i.assert_node(SyntaxKind.NameExpression)
        i.assert_token(SyntaxKind.IdentifierToken, "a")
        i.assert_token(op1, op1_text)
        i.assert_node(SyntaxKind.NameExpression)
        i.assert_token(SyntaxKind.IdentifierToken, "b")
        i.assert_token(op2, op2_text)
        i.assert_node(SyntaxKind.NameExpression)
        i.assert_token(SyntaxKind.IdentifierToken, "c")
        i.assert_at_end()
    else:
        i = AssertingIterator(expression)

        i.assert_node(SyntaxKind.BinaryExpression)
        i.assert_node(SyntaxKind.NameExpression)
        i.assert_token(SyntaxKind.IdentifierToken, "a")
        i.assert_token(op1, op1_text)
        i.assert_node(SyntaxKind.BinaryExpression)
        i.assert_node(SyntaxKind.NameExpression)
        i.assert_token(SyntaxKind.IdentifierToken, "b")
        i.assert_token(op2, op2_text)
        i.assert_node(SyntaxKind.NameExpression)
        i.assert_token(SyntaxKind.IdentifierToken, "c")
        i.assert_at_end()
