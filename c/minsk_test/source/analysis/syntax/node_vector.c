#include "minsk_test/analysis/syntax/node_vector.h"
#include <assert.h>
#include <stdlib.h>

void syntax_node_vector_init(syntax_node_vector_t* vector) {
  vector->data = malloc(sizeof(const syntax_node_t*) * 8);
  vector->length = 0;
  vector->capacity = 8;
}

void syntax_node_vector_push(
    syntax_node_vector_t* vector,
    const syntax_node_t* node
) {
  if (vector->length == vector->capacity) {
    vector->capacity *= 2;
    const syntax_node_t** new_data =
        realloc(vector->data, sizeof(const syntax_node_t*) * vector->capacity);
    assert(new_data != NULL);
    vector->data = new_data;
  }
  vector->data[vector->length] = node;
  vector->length += 1;
}

const syntax_node_t* syntax_node_vector_pop(syntax_node_vector_t* vector) {
  assert(vector->length > 0);
  vector->length -= 1;
  return vector->data[vector->length];
}

void syntax_node_vector_free(syntax_node_vector_t* vector) {
  free(vector->data);
}
