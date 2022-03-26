#include "minsk_test/analysis/syntax/kind_vector.h"
#include <assert.h>
#include <stdlib.h>

void syntax_kind_vector_init(syntax_kind_vector_t *vector) {
  vector->data = malloc(sizeof(syntax_kind_t) * 8);
  vector->length = 0;
  vector->capacity = 8;
}

void syntax_kind_vector_push(syntax_kind_vector_t *vector, syntax_kind_t kind) {
  if (vector->length == vector->capacity) {
    vector->capacity *= 2;
    syntax_kind_t *new_data =
        realloc(vector->data, sizeof(syntax_kind_t) * vector->capacity);
    assert(new_data != NULL);
    vector->data = new_data;
  }
  vector->data[vector->length] = kind;
  vector->length += 1;
}

void syntax_kind_vector_free(syntax_kind_vector_t *vector) {
  free(vector->data);
}
