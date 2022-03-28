#include "minsk/analysis/symbols.h"
#include "minsk/analysis/symbol.h"
#include <assert.h>
#include <stdlib.h>

void variable_symbol_vector_init(variable_symbol_vector_t *vector) {
  vector->data = NULL;
  vector->length = 0;
  vector->capacity = 0;
}

void variable_symbol_vector_push(variable_symbol_vector_t *vector,
                                 variable_symbol_t value) {
  if (vector->length == vector->capacity) {
    vector->capacity = vector->capacity == 0 ? 4 : vector->capacity * 2;
    variable_symbol_t *new_data =
        realloc(vector->data, sizeof(variable_symbol_t) * vector->capacity);
    assert(vector->data != NULL);
    vector->data = new_data;
  }
  vector->data[vector->length] = value;
  vector->length++;
}

void variable_symbol_vector_free(variable_symbol_vector_t *vector) {
  for (size_t i = 0; i < vector->length; i++) {
    variable_symbol_free(&vector->data[i]);
  }
  free(vector->data);
  vector->data = NULL;
  vector->length = 0;
  vector->capacity = 0;
}
