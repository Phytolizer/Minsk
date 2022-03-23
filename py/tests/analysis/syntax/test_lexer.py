from dataclasses import dataclass
from typing import Iterable

import pytest

from minsk.analysis.syntax import facts
from minsk.analysis.syntax.kind import SyntaxKind
from minsk.analysis.syntax.parser import SyntaxTree


@dataclass(frozen=True)
class SimpleToken:
    kind: SyntaxKind
    text: str


def get_tokens() -> Iterable[SimpleToken]:
    for static_kind in (
        kind for kind in SyntaxKind if facts.get_text(kind) is not None
    ):
        yield SimpleToken(static_kind, facts.get_text(static_kind))
    yield SimpleToken(SyntaxKind.NumberToken, "1")
    yield SimpleToken(SyntaxKind.NumberToken, "123")
    yield SimpleToken(SyntaxKind.IdentifierToken, "a")
    yield SimpleToken(SyntaxKind.IdentifierToken, "abc")


def get_separators() -> Iterable[SimpleToken]:
    yield SimpleToken(SyntaxKind.WhitespaceToken, " ")
    yield SimpleToken(SyntaxKind.WhitespaceToken, "  ")
    yield SimpleToken(SyntaxKind.WhitespaceToken, "\r")
    yield SimpleToken(SyntaxKind.WhitespaceToken, "\n")
    yield SimpleToken(SyntaxKind.WhitespaceToken, "\r\n")


def requires_separator(t1: SimpleToken, t2: SimpleToken) -> bool:
    t1_is_keyword = str(t1.kind).endswith("Keyword")
    t2_is_keyword = str(t2.kind).endswith("Keyword")

    if (t1.kind == SyntaxKind.IdentifierToken or t1_is_keyword) and (
        t2.kind == SyntaxKind.IdentifierToken or t2_is_keyword
    ):
        return True

    if t1.kind == SyntaxKind.NumberToken and t2.kind == SyntaxKind.NumberToken:
        return True

    if (t1.kind == SyntaxKind.BangToken or t1.kind == SyntaxKind.EqualsToken) and (
        t2.kind == SyntaxKind.EqualsToken or t2.kind == SyntaxKind.EqualsEqualsToken
    ):
        return True

    return False


def get_token_pairs() -> Iterable[tuple[SimpleToken, SimpleToken]]:
    for t1 in get_tokens():
        for t2 in get_tokens():
            if not requires_separator(t1, t2):
                yield t1, t2


def get_token_pairs_with_separator() -> Iterable[
    tuple[SimpleToken, SimpleToken, SimpleToken]
]:
    for t1 in get_tokens():
        for t2 in get_tokens():
            if requires_separator(t1, t2):
                for sep in get_separators():
                    yield t1, sep, t2


@pytest.mark.parametrize("t", get_tokens())
def test_lexes_token(t: SimpleToken):
    tokens = SyntaxTree.parse_tokens(t.text)
    assert len(tokens) == 1
    assert tokens[0].kind == t.kind
    assert tokens[0].text == t.text


@pytest.mark.parametrize("t1,t2", get_token_pairs())
def test_lexes_token_pairs(t1: SimpleToken, t2: SimpleToken):
    text = t1.text + t2.text
    tokens = SyntaxTree.parse_tokens(text)
    assert len(tokens) == 2
    assert tokens[0].kind == t1.kind
    assert tokens[0].text == t1.text
    assert tokens[1].kind == t2.kind
    assert tokens[1].text == t2.text


@pytest.mark.parametrize("t1,sep,t2", get_token_pairs_with_separator())
def test_lexes_token_pairs_with_separator(
    t1: SimpleToken, sep: SimpleToken, t2: SimpleToken
):
    text = t1.text + sep.text + t2.text
    tokens = SyntaxTree.parse_tokens(text)
    assert len(tokens) == 3
    assert tokens[0].kind == t1.kind
    assert tokens[0].text == t1.text
    assert tokens[1].kind == sep.kind
    assert tokens[1].text == sep.text
    assert tokens[2].kind == t2.kind
    assert tokens[2].text == t2.text
