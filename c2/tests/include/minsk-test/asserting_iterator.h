#pragma once

#include <arena.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/syntax/ast/node.h>
#include <minsk/code_analysis/syntax/ast/node_type.h>
#include <minsk/code_analysis/syntax/kind.h>
#include <stddef.h>

typedef struct
{
  Arena * arena;
  minsk_syntax_node_buf_t nodes;
  size_t i;
} asserting_iterator_t;

extern asserting_iterator_t
asserting_iterator_new(Arena * arena, minsk_syntax_node_t root);

extern void
asserting_iterator_assert_node(
  asserting_iterator_t * it,
  string_t name,
  minsk_syntax_node_type_t type
);

extern void
asserting_iterator_assert_token(
  asserting_iterator_t * it,
  string_t name,
  minsk_syntax_kind_t kind,
  string_t text
);

extern void
asserting_iterator_assert_done(asserting_iterator_t it, string_t name);
