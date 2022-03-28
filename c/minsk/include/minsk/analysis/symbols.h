#pragma once

#include "minsk/analysis/symbol.h"

typedef struct {
  variable_symbol_t *data;
  size_t length;
  size_t capacity;
} variable_symbol_vector_t;

void variable_symbol_vector_init(variable_symbol_vector_t *vector);
void variable_symbol_vector_push(variable_symbol_vector_t *vector,
                                   variable_symbol_t value);
void variable_symbol_vector_free(variable_symbol_vector_t *vector);
