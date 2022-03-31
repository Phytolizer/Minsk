#pragma once

#include "minsk/analysis/syntax/node.h"
#include "node_vector.h"
#include <stddef.h>

typedef struct {
  syntax_node_vector_t nodes;
  size_t position;
} asserting_iterator_t;

void asserting_iterator_init(
    asserting_iterator_t* iterator, const syntax_node_t* root);
void asserting_iterator_assert_node(
    asserting_iterator_t* iterator, syntax_kind_t kind);
void asserting_iterator_assert_token(
    asserting_iterator_t* iterator, syntax_kind_t kind, const char* text);
void asserting_iterator_free(asserting_iterator_t* iterator);
