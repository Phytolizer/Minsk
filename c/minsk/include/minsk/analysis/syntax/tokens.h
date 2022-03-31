#pragma once

#include "minsk/analysis/syntax/token.h"

typedef struct {
  syntax_token_t* data;
  size_t length;
  size_t capacity;
} syntax_token_vector_t;

void syntax_token_vector_init(syntax_token_vector_t* vector);
void syntax_token_vector_free(syntax_token_vector_t* vector);
void syntax_token_vector_push(
    syntax_token_vector_t* vector, syntax_token_t token);
