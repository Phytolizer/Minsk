#pragma once

#include "minsk/analysis/syntax/node.h"
#include <stddef.h>

typedef struct {
  syntax_node_t base;
} statement_syntax_t;

typedef struct {
  statement_syntax_t** data;
  size_t length;
  size_t capacity;
} statement_syntax_vector_t;

void statement_syntax_vector_init(statement_syntax_vector_t* vector);
void statement_syntax_vector_push(
    statement_syntax_vector_t* vector,
    statement_syntax_t* value
);
void statement_syntax_vector_free(statement_syntax_vector_t* vector);
