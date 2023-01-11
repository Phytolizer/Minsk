#include "minsk/analysis/syntax/tokens.h"
#include "minsk/analysis/syntax/token.h"
#include <assert.h>
#include <stdlib.h>

void syntax_token_vector_init(syntax_token_vector_t* vector) {
  vector->data = malloc(sizeof(syntax_token_t) * 8);
  vector->length = 0;
  vector->capacity = 8;
}

void syntax_token_vector_free(syntax_token_vector_t* vector) {
  free(vector->data);
}

void syntax_token_vector_push(
    syntax_token_vector_t* vector,
    syntax_token_t token
) {
  if (vector->length == vector->capacity) {
    vector->capacity *= 2;
    syntax_token_t* new_data =
        realloc(vector->data, sizeof(syntax_token_t) * vector->capacity);
    assert(new_data != NULL);
    vector->data = new_data;
  }
  vector->data[vector->length] = token;
  vector->length += 1;
}
