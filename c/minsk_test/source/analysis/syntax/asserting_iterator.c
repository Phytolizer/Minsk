#include "minsk_test/analysis/syntax/asserting_iterator.h"
#include "check.h"
#include "minsk/analysis/syntax/node.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk_test/analysis/syntax/node_vector.h"
#include <assert.h>

void asserting_iterator_init(
    asserting_iterator_t* iterator, const syntax_node_t* root) {
  iterator->position = 0;
  syntax_node_vector_t stack;
  syntax_node_vector_init(&stack);
  syntax_node_vector_push(&stack, root);
  syntax_node_vector_init(&iterator->nodes);

  while (stack.length > 0) {
    const syntax_node_t* node = syntax_node_vector_pop(&stack);
    syntax_node_vector_push(&iterator->nodes, node);

    syntax_node_children_t children = syntax_node_children(node);
    for (size_t i = children.length; i > 0; i--) {
      syntax_node_vector_push(&stack, children.data[i - 1]);
    }
    syntax_node_children_free(&children);
  }

  syntax_node_vector_free(&stack);
}

void asserting_iterator_assert_node(
    asserting_iterator_t* iterator, syntax_kind_t kind) {
  // Assert that the current node is not a token, and that it has the correct
  // kind.
  ck_assert_int_lt(iterator->position, iterator->nodes.length);
  const syntax_node_t* node = iterator->nodes.data[iterator->position];
  ck_assert(!node->is_token);
  ck_assert_int_eq(node->kind, kind);
  iterator->position += 1;
}

void asserting_iterator_assert_token(
    asserting_iterator_t* iterator, syntax_kind_t kind, const char* text) {
  // Assert that the current node is a token, and that it has the correct kind
  // and text.
  ck_assert_int_lt(iterator->position, iterator->nodes.length);
  const syntax_node_t* node = iterator->nodes.data[iterator->position];
  ck_assert(node->is_token);
  ck_assert_int_eq(node->kind, kind);
  ck_assert_str_eq(((const syntax_token_t*)node)->text, text);
  iterator->position += 1;
}

void asserting_iterator_free(asserting_iterator_t* iterator) {
  ck_assert_int_eq(iterator->position, iterator->nodes.length);
  syntax_node_vector_free(&iterator->nodes);
}
