#pragma once

#include "lexer.h"
#include "token.h"

typedef struct {
  lexer_t lexer;
  syntax_token_t *data;
  size_t length;
  size_t capacity;
} peek_buffer_t;

void peek_buffer_init(peek_buffer_t *buffer, const char *text);
syntax_token_t *peek_buffer_peek(peek_buffer_t *buffer, size_t offset);
syntax_token_t peek_buffer_pop(peek_buffer_t *buffer);
void peek_buffer_free(peek_buffer_t *buffer);
