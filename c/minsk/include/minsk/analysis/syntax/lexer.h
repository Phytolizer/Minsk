#pragma once

#include "sds.h"
#include "token.h"

typedef struct {
  sds text;
  int position;
} lexer_t;

void lexer_init(lexer_t *lexer, sds text);
syntax_token_t lexer_next_token(lexer_t *lexer);
