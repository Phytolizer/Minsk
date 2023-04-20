#include "minsk-test/asserting_iterator.h"

#include "minsk-test/tau-ext.h"

static minsk_syntax_node_buf_t
flatten(Arena * arena, minsk_syntax_node_t root)
{
  minsk_syntax_node_buf_t stack = BUF_INIT;
  BUF_PUSH_ARENA(arena, &stack, root);
  minsk_syntax_node_buf_t out = BUF_INIT;

  while (stack.len > 0)
  {
    minsk_syntax_node_t n = BUF_POP(&stack);
    BUF_PUSH(&out, n);
    minsk_syntax_node_buf_t children = minsk_syntax_node_children(arena, n);
    for (size_t i = children.len; i > 0; i--)
    {
      BUF_PUSH_ARENA(arena, &stack, children.ptr[i - 1]);
    }
  }
  return out;
}

extern asserting_iterator_t
asserting_iterator_new(Arena * arena, minsk_syntax_node_t root)
{
  asserting_iterator_t it = {.arena = arena, .nodes = flatten(arena, root)};
  return it;
}

extern void
asserting_iterator_assert_node(
  asserting_iterator_t * it,
  string_t name,
  minsk_syntax_node_type_t type
)
{
  EXTREQUIRE(
    it->i < it->nodes.len,
    STRING_FMT ": not enough nodes",
    STRING_ARG(name)
  );
  EXTCHECK(
    it->nodes.ptr[it->i].type == type,
    STRING_FMT ": wrong type " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(
      minsk_syntax_node_type_display_name(it->arena, it->nodes.ptr[it->i].type)
    ),
    STRING_ARG(minsk_syntax_node_type_display_name(it->arena, type))
  );
  it->i++;
}

extern void
asserting_iterator_assert_token(
  asserting_iterator_t * it,
  string_t name,
  minsk_syntax_kind_t kind,
  string_t text
)
{
  EXTREQUIRE(
    it->i < it->nodes.len,
    STRING_FMT ": not enough nodes",
    STRING_ARG(name)
  );
  EXTREQUIRE(
    it->nodes.ptr[it->i].type == MINSK_SYNTAX_NODE_TYPE_TOKEN,
    STRING_FMT ": wrong type " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(
      minsk_syntax_node_type_display_name(it->arena, it->nodes.ptr[it->i].type)
    ),
    STRING_ARG(minsk_syntax_node_type_display_name(
      it->arena,
      MINSK_SYNTAX_NODE_TYPE_TOKEN
    ))
  );
  minsk_syntax_token_t token = it->nodes.ptr[it->i].token;
  EXTCHECK(
    token.kind == kind,
    STRING_FMT ": wrong kind " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(minsk_syntax_kind_display_name(it->arena, token.kind)),
    STRING_ARG(minsk_syntax_kind_display_name(it->arena, kind))
  );
  EXTCHECK(
    STRING_EQUAL(token.text, text),
    STRING_FMT ": wrong text '" STRING_FMT "', expected '" STRING_FMT "'",
    STRING_ARG(name),
    STRING_ARG(token.text),
    STRING_ARG(text)
  );
  it->i++;
}

extern void
asserting_iterator_assert_done(asserting_iterator_t it, string_t name)
{
  EXTREQUIRE(
    it.i == it.nodes.len,
    STRING_FMT ": too many nodes (%zu > %zu)",
    STRING_ARG(name),
    it.i,
    it.nodes.len
  );
}
