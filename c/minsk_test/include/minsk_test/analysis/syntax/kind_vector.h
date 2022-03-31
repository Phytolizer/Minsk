#pragma once

#include "minsk/analysis/syntax/kind.h"

typedef struct {
  syntax_kind_t* data;
  size_t length;
  size_t capacity;
} syntax_kind_vector_t;

void syntax_kind_vector_init(syntax_kind_vector_t* vector);
void syntax_kind_vector_push(syntax_kind_vector_t* vector, syntax_kind_t kind);
void syntax_kind_vector_free(syntax_kind_vector_t* vector);
