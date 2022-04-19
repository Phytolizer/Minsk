#include "minsk/analysis/syntax/node/statement.h"
#include "minsk/analysis/syntax/node.h"
#include <assert.h>
#include <stdlib.h>

void statement_syntax_vector_init(statement_syntax_vector_t* vector) {
  vector->data = NULL;
  vector->length = 0;
  vector->capacity = 0;
}

void statement_syntax_vector_push(
    statement_syntax_vector_t* vector, statement_syntax_t* value) {
  if (vector->length == vector->capacity) {
    vector->capacity = vector->capacity == 0 ? 1 : vector->capacity * 2;
    statement_syntax_t** new_data =
        realloc(vector->data, sizeof(statement_syntax_t*) * vector->capacity);
    assert(new_data != NULL);
    vector->data = new_data;
  }
  vector->data[vector->length] = value;
  vector->length += 1;
}

void statement_syntax_vector_free(statement_syntax_vector_t* vector) {
  for (size_t i = 0; i < vector->length; i++) {
    syntax_node_free((syntax_node_t*)vector->data[i]);
  }
  free(vector->data);
  statement_syntax_vector_init(vector);
}
