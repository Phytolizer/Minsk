#include <arena.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/syntax/ast/node_type.h>
#include <minsk/code_analysis/syntax/facts.h>
#include <minsk/code_analysis/syntax/kind.h>
#include <minsk/code_analysis/syntax/tree.h>
#include <tau/tau.h>

#include "minsk-test/asserting_iterator.h"

static void
test_binary_expression_honors_precedence(
  Arena * arena,
  string_t name,
  minsk_syntax_kind_t op1,
  minsk_syntax_kind_t op2
)
{
  minsk_syntax_facts_precedence_t op1_precedence =
    minsk_syntax_facts_binary_operator_precedence(op1);
  minsk_syntax_facts_precedence_t op2_precedence =
    minsk_syntax_facts_binary_operator_precedence(op2);
  string_t op1_text = minsk_syntax_facts_get_text(op1);
  string_t op2_text = minsk_syntax_facts_get_text(op2);
  string_t text = string_printf_arena(
    arena,
    "a " STRING_FMT " b " STRING_FMT " c",
    STRING_ARG(op1_text),
    STRING_ARG(op2_text)
  );
  minsk_syntax_tree_t tree = minsk_syntax_tree_parse(arena, text);

  if (op1_precedence >= op2_precedence)
  {
    asserting_iterator_t it = asserting_iterator_new(arena, tree.root);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION
    );
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION
    );
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("a")
    );
    asserting_iterator_assert_token(&it, name, op1, op1_text);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("b")
    );
    asserting_iterator_assert_token(&it, name, op2, op2_text);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("c")
    );
    asserting_iterator_assert_done(it, name);
  }
  else
  {
    asserting_iterator_t it = asserting_iterator_new(arena, tree.root);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION
    );
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("a")
    );
    asserting_iterator_assert_token(&it, name, op1, op1_text);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION
    );
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("b")
    );
    asserting_iterator_assert_token(&it, name, op2, op2_text);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("c")
    );
    asserting_iterator_assert_done(it, name);
  }
}

TEST(parser, binary_expression_honors_precedence)
{
  Arena test_arena = {0};
  for (minsk_syntax_kind_iterator_t it1 =
         minsk_syntax_facts_get_binary_operator_kinds();
       it1.next(&it1);)
  {
    for (minsk_syntax_kind_iterator_t it2 =
           minsk_syntax_facts_get_binary_operator_kinds();
         it2.next(&it2);)
    {
      string_t name = string_printf_arena(
        &test_arena,
        STRING_FMT ", " STRING_FMT,
        STRING_ARG(minsk_syntax_kind_display_name(&test_arena, it1.curr)),
        STRING_ARG(minsk_syntax_kind_display_name(&test_arena, it2.curr))
      );
      test_binary_expression_honors_precedence(
        &test_arena,
        name,
        it1.curr,
        it2.curr
      );
    }
  }
  arena_free(&test_arena);
}

static void
test_unary_expression_honors_precedence(
  Arena * arena,
  string_t name,
  minsk_syntax_kind_t op1,
  minsk_syntax_kind_t op2
)
{
  minsk_syntax_facts_precedence_t op1_precedence =
    minsk_syntax_facts_unary_operator_precedence(op1);
  minsk_syntax_facts_precedence_t op2_precedence =
    minsk_syntax_facts_binary_operator_precedence(op2);
  string_t op1_text = minsk_syntax_facts_get_text(op1);
  string_t op2_text = minsk_syntax_facts_get_text(op2);
  string_t text = string_printf_arena(
    arena,
    STRING_FMT " a " STRING_FMT " b",
    STRING_ARG(op1_text),
    STRING_ARG(op2_text)
  );
  minsk_syntax_tree_t tree = minsk_syntax_tree_parse(arena, text);

  if (op1_precedence >= op2_precedence)
  {
    asserting_iterator_t it = asserting_iterator_new(arena, tree.root);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION
    );
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_UNARY_EXPRESSION
    );
    asserting_iterator_assert_token(&it, name, op1, op1_text);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("a")
    );
    asserting_iterator_assert_token(&it, name, op2, op2_text);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("b")
    );
    asserting_iterator_assert_done(it, name);
  }
  else
  {
    asserting_iterator_t it = asserting_iterator_new(arena, tree.root);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_UNARY_EXPRESSION
    );
    asserting_iterator_assert_token(&it, name, op1, op1_text);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION
    );
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("a")
    );
    asserting_iterator_assert_token(&it, name, op2, op2_text);
    asserting_iterator_assert_node(
      &it,
      name,
      MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION
    );
    asserting_iterator_assert_token(
      &it,
      name,
      MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN,
      STRING_REF("b")
    );
    asserting_iterator_assert_done(it, name);
  }
}

TEST(parser, unary_expression_honors_precedence)
{
  Arena test_arena = {0};
  for (minsk_syntax_kind_iterator_t it1 =
         minsk_syntax_facts_get_unary_operator_kinds();
       it1.next(&it1);)
  {
    for (minsk_syntax_kind_iterator_t it2 =
           minsk_syntax_facts_get_binary_operator_kinds();
         it2.next(&it2);)
    {
      string_t name = string_printf_arena(
        &test_arena,
        STRING_FMT ", " STRING_FMT,
        STRING_ARG(minsk_syntax_kind_display_name(&test_arena, it1.curr)),
        STRING_ARG(minsk_syntax_kind_display_name(&test_arena, it2.curr))
      );
      test_unary_expression_honors_precedence(
        &test_arena,
        name,
        it1.curr,
        it2.curr
      );
    }
  }
  arena_free(&test_arena);
}
