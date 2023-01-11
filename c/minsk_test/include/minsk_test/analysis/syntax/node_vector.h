#pragma once

#include "minsk/analysis/syntax/node.h"
#include <stddef.h>

typedef struct {
  const syntax_node_t** data;
  size_t length;
  size_t capacity;
} syntax_node_vector_t;

void syntax_node_vector_init(syntax_node_vector_t* vector);
void syntax_node_vector_push(
    syntax_node_vector_t* vector,
    const syntax_node_t* node
);
const syntax_node_t* syntax_node_vector_pop(syntax_node_vector_t* vector);
void syntax_node_vector_free(syntax_node_vector_t* vector);
