#include "minsk/analysis/syntax/peek_buffer.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/analysis/text/source.h"
#include <assert.h>
#include <stdlib.h>
static void peek_buffer_push(peek_buffer_t* buffer) {
  if (buffer->length == buffer->capacity) {
    buffer->capacity = buffer->capacity * 2 + 1;
    syntax_token_t* new_data =
        realloc(buffer->data, sizeof(syntax_token_t) * buffer->capacity);
    if (new_data == NULL) {
      assert(false && "memory allocation failure");
    }
    buffer->data = new_data;
  }
  syntax_token_t tok = lexer_next_token(&buffer->lexer);
  while (tok.base.kind == syntax_kind_whitespace_token ||
         tok.base.kind == syntax_kind_bad_token) {
    token_free(&tok);
    tok = lexer_next_token(&buffer->lexer);
  }
  buffer->data[buffer->length] = tok;
  buffer->length++;
}
void peek_buffer_init(peek_buffer_t* buffer, source_text_t text) {
  lexer_init(&buffer->lexer, text);
  buffer->data = NULL;
  buffer->length = 0;
  buffer->capacity = 0;
}
syntax_token_t* peek_buffer_peek(peek_buffer_t* buffer, size_t offset) {
  while (buffer->length < offset + 1) {
    peek_buffer_push(buffer);
  }
  return &buffer->data[offset];
}
syntax_token_t peek_buffer_pop(peek_buffer_t* buffer) {
  if (buffer->length == 0) {
    peek_buffer_push(buffer);
  }
  syntax_token_t result = buffer->data[0];
  buffer->length -= 1;
  for (size_t i = 0; i < buffer->length; i++) {
    buffer->data[i] = buffer->data[i + 1];
  }
  return result;
}
void peek_buffer_free(peek_buffer_t* buffer) {
  for (size_t i = 0; i < buffer->length; i++) {
    token_free(&buffer->data[i]);
  }
  free(buffer->data);
  buffer->data = NULL;
  lexer_free(&buffer->lexer);
}
